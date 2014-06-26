use 5.006;
use strict;
use warnings;

package Hash::Ordered;
# ABSTRACT: A compact, pure-Perl ordered hash class
# VERSION

use Carp ();
use List::Util 1.09 ();

use constant {
    _DATA => 0,
    _KEYS => 1,
};

sub new {
    my ( $class, @pairs ) = @_;
    Carp::croak("new() requires key-value pairs") unless @pairs % 2 == 0;

    my $self = [ {@pairs}, [ map { $_ % 2 == 0 ? ( $pairs[$_] ) : () } 0 .. $#pairs ] ];

    return bless $self, $class;
}

sub clone {
    my ($self) = @_;
    my $clone = [ { %{ $self->[_DATA] } }, [ @{ $self->[_KEYS] } ] ];
    return bless $clone, ref $self;
}

sub keys {
    my ($self) = @_;
    return @{ $self->[_KEYS] };
}

sub values {
    my ($self) = @_;
    return map { $self->[_DATA]{$_} } @{ $self->[_KEYS] };
}

sub exists {
    my ( $self, $key ) = @_;
    return exists $self->[_DATA]{$key};
}

sub get {
    my ( $self, $key ) = @_;
    return $self->[_DATA]{$key};
}

sub set {
    my ( $self, $key, $value ) = @_;
    if ( !exists $self->[_DATA]{$key} ) {
        push @{ $self->[_KEYS] }, $key;
    }
    return $self->[_DATA]{$key} = $value;
}

sub delete {
    my ( $self, $key ) = @_;
    if ( exists $self->[_DATA]{$key} ) {
        # XXX could put an index on this later if linear search is too slow
        my $r = $self->[_KEYS];
        my $i = List::Util::first { $r->[$_] eq $key } 0 .. $#$r;
        splice @$r, $i, 1;
        return delete $self->[_DATA]{$key};
    }
    return undef;
}

sub push {
    my ( $self, @pairs ) = @_;
    while (@pairs) {
        my ( $k, $v ) = splice( @pairs, 0, 2 );
        if ( exists $self->[_DATA]{$k} ) {
            # splice out key
            # XXX could put an index on this later if linear search is too slow
            my $r = $self->[_KEYS];
            my $i = List::Util::first { $r->[$_] eq $k } 0 .. $#$r;
            splice @$r, $i, 1;
        }
        push @{ $self->[_KEYS] }, $k;
        $self->[_DATA]{$k} = $v;
    }
    return scalar $self->keys;
}

sub pop {
    my ($self) = @_;
    my $key = pop @{ $self->[_KEYS] };
    return $key, delete $self->[_DATA]{$key};
}

sub unshift {
    my ( $self, @pairs ) = @_;
    while (@pairs) {
        my ( $k, $v ) = splice( @pairs, -2, 2 );
        if ( exists $self->[_DATA]{$k} ) {
            # splice out key
            # XXX could put an index on this later if linear search is too slow
            my $r = $self->[_KEYS];
            my $i = List::Util::first { $r->[$_] eq $k } 0 .. $#$r;
            splice @$r, $i, 1;
        }
        unshift @{ $self->[_KEYS] }, $k;
        $self->[_DATA]{$k} = $v;
    }
    return scalar $self->keys;
}

sub shift {
    my ($self) = @_;
    my $key = shift @{ $self->[_KEYS] };
    return $key, delete $self->[_DATA]{$key};
}

sub as_list {
    my ($self) = @_;
    return map { ; $_ => $self->[_DATA]{$_} } @{ $self->[_KEYS] };
}

sub iterator {
    my ($self) = @_;
    my @keys   = @{ $self->[_KEYS] };
    my $data   = $self->[_DATA];
    return sub {
        return unless @keys;
        return $data->{ shift @keys };
    };
}

1;

=for Pod::Coverage BUILD

=head1 SYNOPSIS

    use Hash::Ordered;

    my $oh = Hash::Ordered->new( a => 1 );

    $oh->get( 'a');
    $oh->set( 'a' => 2 );

    $oh->exists('a');
    $oh->delete('a');

    @keys  = $oh->keys;
    @vals  = $oh->values;
    @pairs = $oh->as_list

    $oh->push( c => 3, d => 4 );
    $oh->unshift( e => 5, f => 6 );

    ($k,$v) = $oh->pop;
    ($k,$v) = $oh->shift;

    $iter = $oh->iterator;
    while( ($k, $v) = $iter->() ) { ... }

=head1 DESCRIPTION

This module might be cool, but you'd never know it from the lack
of documentation.

=head1 USAGE

Good luck!

=head1 SEE ALSO

=for :list
* Maybe other modules do related things.

=cut

# vim: ts=4 sts=4 sw=4 et:
