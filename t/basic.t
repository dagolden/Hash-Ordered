use 5.006;
use strict;
use warnings;
use Test::More 0.96;
use Test::FailWarnings;
use Test::Deep '!blessed';
use Test::Fatal;
binmode( Test::More->builder->$_, ":utf8" )
  for qw/output failure_output todo_output/;

use Hash::Ordered;

use constant HO => "Hash::Ordered";

subtest "constructors" => sub {

    my $hash;

    $hash = new_ok( HO, [], "new()" );

    $hash = new_ok( HO, [ a => 1, b => 2 ], "new( \@pairs )" );
    cmp_deeply( [ $hash->keys ],   [qw/a b/], "keys ordered as expected" );
    cmp_deeply( [ $hash->values ], [qw/1 2/], "values ordered as expected" );

    like(
        exception { HO->new("a") },
        qr/requires key-value pairs/,
        "unbalanced args throws exception"
    );

    my $clone = $hash->clone;
    cmp_deeply( $clone, $hash, "clone() returns copy" );

    my $same = $hash->clone( $hash->keys );
    cmp_deeply( [ $same->as_list ], [ $hash->as_list ], "clone( keys )" );

    my $rev = $hash->clone( reverse $hash->keys );
    cmp_deeply( [ $rev->as_list ], [ b => 2, a => 1 ], "clone( reverse keys )" );

    my $filter = $hash->clone('a');
    cmp_deeply( [ $filter->as_list ], [ a => 1 ], "clone( 'a' )" );

    my $extra = $hash->clone( 'c', 'a' );
    cmp_deeply( [ $extra->as_list ], [ c => undef, a => 1 ], "clone( 'c', 'a' )" );

};

subtest "boolean" => sub {

    my $hash = new_ok( HO, [], "new()" );
    ok( !$hash, "empty hash is false" );
    $hash->set( a => 1 );
    ok( $hash, "non-empty hash is true" );

};

subtest "element methods" => sub {

    my $hash = new_ok( HO, [], "new()" );

    ok( !$hash->exists("a"), "exists is false for non-existing element" );
    is( $hash->get("a"), undef, "get on non-existing element returns undef" );
    is( $hash->set( "a", 1 ), 1, "set on non-existing element returns new value" );
    is( $hash->get("a"), 1, "get on existing element returns value" );
    ok( $hash->exists("a"), "exists is true for existing element" );

    is( $hash->set( "b", 2 ), 2, "set another key" );
    cmp_deeply( [ $hash->keys ],   [qw/a b/], "keys ordered as expected" );
    cmp_deeply( [ $hash->values ], [qw/1 2/], "values ordered as expected" );

    is( $hash->delete("a"), 1,     "delete existing key returns old value" );
    is( $hash->delete("z"), undef, "delete non-existing key returns undef" );

    is( $hash->set( "c", 3 ), 3, "set another key" );
    cmp_deeply( [ $hash->keys ],   [qw/b c/], "keys ordered as expected" );
    cmp_deeply( [ $hash->values ], [qw/2 3/], "values ordered as expected" );

};

subtest "output and iteration" => sub {

    my $hash = new_ok( HO, [ 'a' .. 'z' ], "new('a'..'z')" );

    cmp_deeply( [ $hash->as_list ], [ 'a' .. 'z' ], "as_list" );

    cmp_deeply(
        [ $hash->as_list(qw/a c g zz/) ],
        [ a => 'b', c => 'd', g => 'h', zz => undef ],
        "as_list( keys )"
    );

    my @slice = $hash->values(qw/a c zz g/);
    cmp_deeply( \@slice, [ 'b', 'd', undef, 'h' ], "values( keys )" );

    my $iter = $hash->iterator;
    my @saw;
    while ( my ( $k, $v ) = $iter->() ) {
        push @saw, $k, $v;
    }
    cmp_deeply( [@saw], [ $hash->as_list ], "iterator walked hash in order" )
      or diag explain \@saw;

    $iter = $hash->iterator( reverse $hash->keys );
    @saw  = ();
    while ( my ( $k, $v ) = $iter->() ) {
        unshift @saw, $k, $v;
    }
    cmp_deeply(
        [@saw],
        [ $hash->as_list ],
        "iterator( reverse keys ) walked hash in expected order"
    ) or diag explain \@saw;
};

subtest "list methods" => sub {

    my $hash = new_ok( HO, [ a => 1 ], "new( a => 1 )" );

    is( $hash->push( b => 2, c => 3 ), 3, "pushing 2 new pairs" );

    cmp_deeply(
        [ $hash->as_list ],
        [ a => 1, b => 2, c => 3 ],
        "hash keys/values correct after pushing new pairs"
    );

    cmp_deeply( [ $hash->pop ], [ c => 3 ], "pop returns last pair" );

    cmp_deeply(
        [ $hash->as_list ],
        [ a => 1, b => 2 ],
        "hash keys/values correct after pop"
    );

    is( $hash->unshift( y => 25, z => 26 ), 4, "unshifting 2 pairs" );

    cmp_deeply(
        [ $hash->as_list ],
        [ y => 25, z => 26, a => 1, b => 2 ],
        "hash keys/values correct after unshifting new pairs"
    );

    cmp_deeply( [ $hash->shift ], [ y => 25 ], "shift returns first pair" );

    ok( $hash->push( z => 42 ), "pushing existing key with new value" );

    cmp_deeply(
        [ $hash->as_list ],
        [ a => 1, b => 2, z => 42 ],
        "hash keys/values correct after pushing existing key"
    );

    ok( $hash->unshift( z => 26 ), "unshifting existing key with new value" );

    cmp_deeply(
        [ $hash->as_list ],
        [ z => 26, a => 1, b => 2 ],
        "hash keys/values correct after unshifting existing key"
    );

    ok( $hash->merge( a => 2, c => 3 ), "merging key-value pairs" );

    cmp_deeply(
        [ $hash->as_list ],
        [ z => 26, a => 2, b => 2, c => 3 ],
        "hash keys/values correct after merging pairs"
    );

};

done_testing;

# vim: ts=4 sts=4 sw=4 et:
