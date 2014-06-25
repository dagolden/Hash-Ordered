use 5.008001;
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

subtest "new" => sub {

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

};

done_testing;
# COPYRIGHT

# vim: ts=4 sts=4 sw=4 et:
