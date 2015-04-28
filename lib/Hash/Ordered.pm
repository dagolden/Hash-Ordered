use 5.006;
use strict;
use warnings;

package Hash::Ordered;
# ABSTRACT: A compact, pure-Perl ordered hash class

our $VERSION = '0.005';

use Carp ();

use constant {
    _DATA => 0,
    _KEYS => 1,
    _INDX => 2,
    _OFFS => 3,
    _GCNT => 4, # garbage count
    _ITER => 5, # for tied hash support
};

use constant {
    _INDEX_THRESHOLD => 25,
    _TOMBSTONE       => \1,
};

# 'overloading.pm' not available until 5.10.1 so emulate with Scalar::Util
BEGIN {
    if ( $] gt '5.010000' ) {
        ## no critic
        eval q{
            sub _stringify { no overloading; "$_[0]" }
            sub _numify { no overloading; 0+$_[0] }
        };
        die $@ if $@; # uncoverable branch true
    }
    else {
        ## no critic
        eval q{
            require Scalar::Util;
            sub _stringify { sprintf("%s=ARRAY(0x%x)",ref($_[0]),Scalar::Util::refaddr($_[0])) }
            sub _numify { Scalar::Util::refaddr($_[0]) }
        };
        die $@ if $@; # uncoverable branch true
    }
}

use overload
  q{""}    => \&_stringify,
  q{0+}    => \&_numify,
  q{bool}  => sub { !!scalar %{ $_[0]->[_DATA] } },
  fallback => 1;

=method new

    $oh = Hash::Ordered->new;
    $oh = Hash::Ordered->new( @pairs );

Constructs an object, with an optional list of key-value pairs.

=cut

sub new {
    my ( $class, @pairs ) = @_;

    return bless [ {}, [], undef, 0, 0 ], $class unless @pairs;

    Carp::croak("new() requires key-value pairs") unless @pairs % 2 == 0;

    # must stringify keys for _KEYS array
    my $self = [
        {@pairs}, [ map { $_ % 2 == 0 ? ("$pairs[$_]") : () } 0 .. $#pairs ],
        undef, 0, 0
    ];

    return bless $self, $class;
}

=method clone

    $oh2 = $oh->clone;
    $oh2 = $oh->clone( @keys );

Creates a shallow copy of an ordered hash object.  If no arguments are
given, it produces an exact copy.  If a list of keys is given, the new
object includes only those keys in the given order.  Keys that aren't
in the original will have the value C<undef>.

=cut

sub clone {
    my $self = shift;
    my @keys = map { "$_" } @_; # stringify keys
    my $clone;
    if (@keys) {
        my %subhash;
        @subhash{@keys} = @{ $self->[_DATA] }{@keys};
        $clone = [ \%subhash, \@keys, undef, 0, 0 ];
    }
    elsif ( $self->[_INDX] ) {
        $clone =
          [ { %{ $self->[_DATA] } }, [ grep !ref($_), @{ $self->[_KEYS] } ], undef, 0, 0 ];
    }
    else {
        $clone =
          [ { %{ $self->[_DATA] } }, [ @{ $self->[_KEYS] } ], undef, 0, 0 ];

    }
    return bless $clone, ref $self;
}

=method keys

    @keys = $oh->keys;
    $size = $oh->keys;

In list context, returns the ordered list of keys.  In scalar context, returns
the number of keys.

=cut

sub keys {
    my ($self) = @_;
    return wantarray
      ? ( grep !ref($_), @{ $self->[_KEYS] } )
      : @{ $self->[_KEYS] } - $self->[_GCNT];
}

=method values

    @values = $oh->values;
    @values = $oh->values( @keys );

Returns an ordered list of values.  If no arguments are given, returns
the ordered values of the entire hash.  If a list of keys is given, returns
values in order corresponding to those keys.  If a key does not exist, C<undef>
will be returned for that value.

=cut

sub values {
    my ( $self, @keys ) = @_;
    return
      map { $self->[_DATA]{$_} } ( @keys ? @keys : grep !ref($_), @{ $self->[_KEYS] } );
}

=method get

    $value = $oh->get("some key");

Returns the value associated with the key, or C<undef> if it does not exist in
the hash.

=cut

sub get {
    return $_[0]->[_DATA]{ $_[1] };
}

=method set

    $oh->set("some key" => "some value");

Associates a value with a key and returns the value.  If the key does not
already exist in the hash, it will be added at the end.

=cut

sub set {
    if ( !exists $_[0]->[_DATA]{ $_[1] } ) {
        push @{ $_[0]->[_KEYS] }, "$_[1]"; # stringify key
        $_[0]->[_INDX]{ $_[1] } = $#{ $_[0]->[_KEYS] } if $_[0]->[_INDX];
    }
    return $_[0]->[_DATA]{ $_[1] } = $_[2];
}

=method exists

    if ( $oh->exists("some key") ) { ... }

Test if some key exists in the hash (without creating it).

=cut

sub exists {
    return exists $_[0]->[_DATA]{ $_[1] };
}

=method delete

    $value = $oh->delete("some key");

Removes a key-value pair from the hash and returns the value.  This
is expensive, as the ordered list of keys has to be updated.

=cut

sub delete {
    my ( $self, $key ) = @_;
    if ( exists $self->[_DATA]{$key} ) {
        my $keys = $self->[_KEYS];

        # JIT an index if hash is "large"
        if ( !$self->[_INDX] && @$keys > _INDEX_THRESHOLD ) {
            my %indx;
            $indx{ $keys->[$_] } = $_ for 0 .. $#{$keys};
            $self->[_INDX] = \%indx;
        }

        if ( $self->[_INDX] ) {

            # tombstone
            $keys->[ delete( $self->[_INDX]{$key} ) + $self->[_OFFS] ] = _TOMBSTONE;

            # GC keys and remove index if more than half keys are tombstone.
            # Index will be recreated if needed on next delete
            if ( ++$self->[_GCNT] > @$keys / 2 ) {
                @{ $self->[_KEYS] } = grep !ref($_), @{ $self->[_KEYS] };
                $self->[_INDX] = undef;
                $self->[_OFFS] = 0;
                $self->[_GCNT] = 0;
            }
            # or maybe garbage collect start of list
            elsif ( ref( $keys->[0] ) ) {
                my $i = 0;
                $i++ while ref( $keys->[$i] );
                splice @$keys, 0, $i;
                $self->[_GCNT] -= $i;
                $self->[_OFFS] -= $i;
            }
            # or maybe garbage collect end of list
            elsif ( ref( $keys->[-1] ) ) {
                my $i = $#{$keys};
                $i-- while ref( $keys->[$i] );
                $self->[_GCNT] -= $#{$keys} - $i;
                splice @$keys, $i + 1;
            }
        }
        else {
            my $i;
            for ( 0 .. $#{$keys} ) {
                if ( $keys->[$_] eq $key ) { $i = $_; last; }
            }
            splice @$keys, $i, 1;
        }

        return delete $self->[_DATA]{$key};
    }
    return undef; ## no critic
}

=method clear

    $oh->clear;

Removes all key-value pairs from the hash.  Returns undef in scalar context
or an empty list in list context.

Added in version 0.003.

=cut

sub clear {
    my ($self) = @_;
    @$self = ( {}, [], undef, 0, 0 );
    return;
}

=method push

    $oh->push( one => 1, two => 2);

Add a list of key-value pairs to the end of the ordered hash.  If a key already
exists in the hash, it will be deleted and re-inserted at the end with the new
value.

Returns the number of keys after the push is complete.

=cut

sub push {
    my ( $self, @pairs ) = @_;
    my ( $data, $keys, $indx ) = @$self;
    while (@pairs) {
        my ( $k, $v ) = splice( @pairs, 0, 2 );
        $self->delete($k) if exists $data->{$k};
        $data->{$k} = $v;
        push @$keys, "$k"; # stringify keys
        $indx->{$k} = $#$keys if $indx;
    }
    return @$keys - $self->[_GCNT];
}

=method pop

    ($key, $value) = $oh->pop;

Removes and returns the last key-value pair in the ordered hash.

=cut

sub pop {
    my ($self) = @_;
    if ( $self->[_INDX] ) {
        my $key = $self->[_KEYS][-1];
        return $key, $self->delete($key);
    }
    else {
        my $key = pop @{ $self->[_KEYS] };
        return $key, delete $self->[_DATA]{$key};
    }
}

=method unshift

    $oh->unshift( one => 1, two => 2 );

Adds a list of key-value pairs to the beginning of the ordered hash.  If a key
already exists, it will be deleted and re-inserted at the beginning with the
new value.

Returns the number of keys after the unshift is complete.

=cut

sub unshift {
    my ( $self, @pairs ) = @_;
    my ( $data, $keys, $indx ) = @$self;
    while (@pairs) {
        my ( $k, $v ) = splice( @pairs, -2, 2 );
        $self->delete($k) if exists $data->{$k};
        $data->{$k} = $v;
        unshift @$keys, "$k"; # stringify keys
        $indx->{$k} = -( ++$self->[_OFFS] ) if $indx;
    }
    return @$keys - $self->[_GCNT];
}

=method shift

    ($key, $value) = $oh->shift;

Removes and returns the first key-value pair in the ordered hash.

=cut

sub shift {
    my ($self) = @_;
    if ( $self->[_INDX] ) {
        my $key = $self->[_KEYS][0];
        return $key, $self->delete($key);
    }
    else {
        my $key = shift @{ $self->[_KEYS] };
        return $key, delete $self->[_DATA]{$key};
    }
}

=method merge

    $oh->merge( one => 1, two => 2 );

Merges a list of key-value pairs into the ordered hash.  If a key already
exists, its value is replaced.  Otherwise, the key-value pair is added at
the end of the hash.

=cut

sub merge {
    my ( $self, @pairs ) = @_;
    while (@pairs) {
        my ( $k, $v ) = splice( @pairs, -2, 2 );
        if ( !exists $self->[_DATA]{$k} ) {
            my $size = CORE::push @{ $self->[_KEYS] }, "$k"; # stringify key
            $self->[_INDX]{$k} = $size - 1 if $self->[_INDX];
        }
        $self->[_DATA]{$k} = $v;
    }
    return @{ $self->[_KEYS] } - $self->[_GCNT];
}

=method as_list

    @pairs = $oh->as_list;
    @pairs = $oh->as_list( @keys );

Returns an ordered list of key-value pairs. If no arguments are given, all
pairs in the hash are returned.  If a list of keys is given, the returned list
includes only those key-value pairs in the given order.  Keys that aren't in
the original will have the value C<undef>.

=cut

sub as_list {
    my ( $self, @keys ) = @_;
    @keys = grep !ref($_), @{ $self->[_KEYS] } unless @keys;
    return map { ; $_ => $self->[_DATA]{$_} } @keys;
}

=method iterator

    $iter = $oh->iterator;
    $iter = $oh->iterator( reverse $oh->keys ); # reverse

    while ( my ($key,$value) = $iter->() ) { ... }

Returns a code reference that returns a single key-value pair (in order) on
each invocation, or the empty list if all keys are visited.

If no arguments are given, the iterator walks the entire hash in order.  If a
list of keys is provided, the iterator walks the hash in that order. Unknown
keys will return C<undef>.

The list of keys to return is set when the iterator is generator.  Keys added
later will not be returned.  Delete keys will return C<undef>.

=cut

sub iterator {
    my ( $self, @keys ) = @_;
    @keys = grep !ref($_), @{ $self->[_KEYS] } unless @keys;
    my $data = $self->[_DATA];
    return sub {
        return unless @keys;
        my $key = CORE::shift(@keys);
        return ( $key => $data->{$key} );
    };
}

#--------------------------------------------------------------------------#
# tied hash support -- slower, but I maybe some thing are more succinct
#--------------------------------------------------------------------------#

{
    no strict 'refs';

    *{ __PACKAGE__ . '::TIEHASH' } = \&new;
    *{ __PACKAGE__ . '::STORE' }   = \&set;
    *{ __PACKAGE__ . '::FETCH' }   = \&get;
    *{ __PACKAGE__ . '::EXISTS' }  = \&exists;
    *{ __PACKAGE__ . '::DELETE' }  = \&delete;
    *{ __PACKAGE__ . '::CLEAR' }   = \&clear;
}

sub FIRSTKEY {
    my ($self) = @_;
    my @keys = grep !ref($_), @{ $self->[_KEYS] };
    $self->[_ITER] = sub {
        return unless @keys;
        return CORE::shift(@keys);
    };
    return $self->[_ITER]->();
}

sub NEXTKEY {
    my ($self) = @_;
    return $self->[_ITER]->();
}

sub SCALAR {
    my ($self) = @_;
    return scalar %{ $self->[_DATA] };
}

1;

=head1 SYNOPSIS

    use Hash::Ordered;

    my $oh = Hash::Ordered->new( a => 1 );

    $oh->get( 'a' );
    $oh->set( 'a' => 2 );

    $oh->exists( 'a' );
    $val = $oh->delete( 'a' );

    @keys  = $oh->keys;
    @vals  = $oh->values;
    @pairs = $oh->as_list

    $oh->push( c => 3, d => 4 );
    $oh->unshift( e => 5, f => 6 );

    ( $k, $v ) = $oh->pop;
    ( $k, $v ) = $oh->shift;

    $iter = $oh->iterator;
    while( ( $k, $v ) = $iter->() ) { ... }

    $copy     = $oh->clone;
    $subset   = $oh->clone( qw/c d/ );
    $reversed = $oh->clone( reverse $oh->keys );

    @value_slice = $oh->values(  qw/c f/ ); # qw/3 6/
    @pairs_slice = $oh->as_list( qw/f e/ ); # qw/f 6 e 5/

=head1 DESCRIPTION

This module implements an ordered hash, meaning that it associates keys with
values like a Perl hash, but keeps the keys in a consistent order.  Because it
is implemented as an object and manipulated with method calls, it is much
slower than a Perl hash.  This is the cost of keeping order.

=head1 OVERLOADING

=head2 Boolean

    if ( $oh ) { ... }

When used in boolean context, a Hash::Ordered object is true if it has any entries
and false otherwise.

=head2 String

    say "$oh";

When used in string context, a Hash::Ordered object stringifies like typical
Perl objects. E.g. C<Hash::Ordered=ARRAY(0x7f815302cac0)>

=head2 Numeric

    $count = 0 + $oh;

When used in numeric context, a Hash::Ordered object numifies as the decimal
representation of its memory address, just like typical Perl objects. E.g.
C<140268162536552>

For the number of keys, call the L</keys> method in scalar context.

=head2 Fallback

Other L<overload> methods are derived from these three, if possible.

=head1 TIED INTERFACE

Using C<tie> is slower than using method calls directly.  But for
compatibility with libraries that can only take hashes, it's available if
you really need it:

    tie my %hash, "Hash::Ordered", @pairs;

If you want to access the underlying object for method calls, use C<tied>:

    tied( %hash )->unshift( @data );

=head1 MOTIVATION

For a long time, I used L<Tie::IxHash> for ordered hashes, but I grew
frustrated with things it lacked, like a cheap way to copy an IxHash object or
a convenient iterator when not using the tied interface.  As I looked at its
implementation, it seemed more complex than I though it needed, with an extra
level of indirection that slows data access.

Given that frustration, I started experimenting with the simplest thing I
thought could work for an ordered hash: a hash of key-value pairs and an array
with key order.

As I worked on this, I also started searching for other modules doing similar
things.  What I found fell broadly into two camps: modules based on tie (even
if they offered an OO interface), and pure OO modules.  They all either lacked
features I deemed necessary or else seemed overly-complex in either
implementation or API.

Hash::Ordered attempts to find the sweet spot with simple implementation,
reasonably good efficiency for most common operations, and a rich, intuitive
API.

=head1 SEE ALSO

This section describes other ordered-hash modules I found on CPAN.  For
benchmarking results, see L<Hash::Ordered::Benchmarks>.

=head2 Tie modules

The following modules offer some sort of tie interface.  I don't like ties, in
general, because of the extra indirection involved over a direct method call, but
if you are willing to pay that penalty, you might want to try one of these.

L<Tie::IxHash> is probably the most well known and includes an OO API.  If its
warts and performance profile aren't a problem, it might serve.

L<Tie::LLHash> I haven't used, but the linked-list implementation might be
worthwhile if you expect to do a lot of deletions.

L<Tie::Hash::Indexed> is implemented in XS and thus seems promising if pure-Perl
isn't a criterion; it often fails tests on Perl 5.18 and above due to the hash
randomization change.

These other modules have very specific designs/limitations and I didn't find
any of them suitable for general purpose use:

=for :list
* L<Tie::Array::AsHash> — array elements split with separator; tie API only
* L<Tie::Hash::Array> — ordered alphabetically; tie API only
* L<Tie::InsertOrderHash> — ordered by insertion; tie API only
* L<Tie::StoredOrderHash> — ordered by last update; tie API only

=head2 Other ordered hash modules

Other modules stick with an object-oriented API, with a wide variety of
implementation approaches.

L<Array::AsHash> is essentially an inverse implementation from Hash::Ordered.
It keeps pairs in an array and uses a hash to index into the array.  I think
this indirection makes hash-like operations slower, but getting the list of
pairs back out is much faster.  It takes an arrayref to initialize, but can
shallow copy it if needed.  I think this is a reasonable alternative if static
construction and listing out contents is more common than individual item
access.

These other modules have restrictions or particularly complicated
implementations (often relying on C<tie>) and thus I didn't think any of them
really suitable for use:

=for :list
* L<Array::Assign> — arrays with named access; restricted keys
* L<Array::OrdHash> — overloads array/hash deref and uses internal tied data
* L<Data::Pairs> — array of key-value hashrefs; allows duplicate keys
* L<Data::OMap> — array of key-value hashrefs; no duplicate keys
* L<Data::XHash> — blessed, tied hashref with doubly-linked-list

=cut

# vim: ts=4 sts=4 sw=4 et:
