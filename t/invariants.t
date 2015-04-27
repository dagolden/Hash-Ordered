use 5.006;
use strict;
use warnings;
use Test::More 0.96;
##use Test::FailWarnings;
use Test::Deep '!blessed';
use Test::Fatal;
binmode( Test::More->builder->$_, ":utf8" )
  for qw/output failure_output todo_output/;

use Hash::Ordered;
use List::Util qw/max min shuffle/;

use constant HO => "Hash::Ordered";

sub _invar {
    my ( $hash, $label ) = @_;

    local $Test::Builder::Level = $Test::Builder::Level + 1;

    subtest $label => sub {

        my ( $data, $keys, $indx, $offs, $gcnt ) = @$hash;

        cmp_deeply(
            [ sort grep !ref($_), @$keys ],
            [ sort keys %$data ],
            "all keys in _DATA are in _KEYS"
        );

        is(
            @$keys - $gcnt,
            scalar( grep !ref($_), @$keys ),
            "_KEYS length minus _GCNT equals number of non-tombstone keys in _KEYS"
        );

        if (%$data) {
            ok( !ref( $keys->[0] ),  "first element of _KEYS is not tombstone" );
            ok( !ref( $keys->[-1] ), "last element of _KEYS is not tombstone" );
        }

        # if indexing has kicked in, invariants change
        if ($indx) {
            pass("has _INDX");
            ok( $gcnt <= @$keys / 2, "no more than half keys elements are tombstones" );
            is( min( values %$indx ) + $offs, 0, "min index value plus offset equals zero" );
            is( max( values %$indx ) + $offs,
                $#{$keys}, "min index value plus offset equals max index of keys" );
        }
        else {
            pass("does not have _INDX");
            is( $offs, 0, "_OFFS is zero without _INDX set" );
            is( $gcnt, 0, "_GCNT is zero without _INDX set" );
        }
    };

}

sub _new {
    my $size = shift;
    return $size ? HO->new( 0 .. $size - 1 ) : HO->new;
}

for my $size ( 0, 8, 98 ) {
    my ( $k, $v, @pairs );

    my $l = "size $size";

    # construct new hash,
    my $h = _new($size);
    _invar( $h, "$l: after creation" );

    # delete some keys
    my @keys = shuffle $h->keys;
    for ( 1 .. 3 ) {
        last unless @keys;
        $h->delete( shift(@keys) );
        _invar( $h, "$l: deleted a key" );
    }

    # clear and recreate
    $h->clear;
    _invar( $h, "$l: after clear" );
    $h = _new($size);
    _invar( $h, "$l: after creation" );

    next unless $size;

    # pop and push
    ( $k, $v ) = $h->pop;
    _invar( $h, "$l: after pop" );
    $h->push( $k, $v );
    _invar( $h, "$l: after push" );

    # shift and unshift
    ( $k, $v ) = $h->shift;
    _invar( $h, "$l: after shift" );
    $h->unshift( $k, $v );
    _invar( $h, "$l: after unshift" );

    # create tombstones at front
    $h->delete(2);
    $h->delete(4);
    _invar( $h, "$l: tombstones at key positions 1 & 2" );
    $h->delete(0);
    _invar( $h, "$l: tombstones at key positions 0" );
    $h->unshift( 0 .. 5 );
    _invar( $h, "$l: keys 0, 2, 4 unshifted" );

    # create tombstones at back
    $h->delete( $size - 6 );
    $h->delete( $size - 4 );
    _invar( $h, "$l: tombstones at key positions -3 & -2" );
    $h->delete( $size - 2 );
    _invar( $h, "$l: tombstones at key positions -1" );
    $h->push( $size - 6 .. $size - 1 );
    _invar( $h, "$l: keys from -3, -2, and -1 pushed back " );

    # unshift and delete
    $h->unshift( "a", "b" );
    _invar( $h, "$l: unshift value" );
    $h->delete("a");
    _invar( $h, "$l: delete unshifted value" );

    # push and delete
    $h->push( "a", "b" );
    _invar( $h, "$l: push value" );
    $h->delete("a");
    _invar( $h, "$l: delete pushed value" );

    # set a reference
    $h->push( $h, 42 );
    _invar( $h, "$l: pushed a reference" );
    $h->delete($h);
    _invar( $h, "$l: deleted a reference" );

    # delete remaining keys randomly
    $h->delete($_) for @keys;
    _invar( $h, "$l: remaining keys deleted" );

}

{
    # construct hash with a reference
    my $ref = [];
    my $h = HO->new( $ref, 42 );
    _invar( $h, "construct hash with reference as key" );
    my $j = $h->clone($ref);
    _invar( $j, "clone hash with reference as key" );
}

done_testing;
