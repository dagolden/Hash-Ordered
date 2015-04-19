#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;

use lib 'lib';
use List::Util qw/shuffle/;
use Math::Random::MT::Auto qw/irand/;
use Benchmark qw( countit );

use Hash::Ordered;

use constant COUNT => 5;
use constant NUMS => [ 10, 100, 1000 ];

STDOUT->autoflush(1);

my %PAIRS = (
    map {
        $_ => [ map { irand() => irand() } 1 .. $_ ]
    } @{ NUMS() }
);

sub timeit {
    my ( $label, $code ) = @_;

    my $res = countit( COUNT, $code );
    my $iter_s = $res->iters / ( $res->cpu_a + 1e-9 );

    printf( "%s: %d/s\n", $label, $iter_s );
}

#--------------------------------------------------------------------------#
# Create with keys
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    timeit(
        "Results for ordered hash creation for $size elements",
        sub {
            my $h = Hash::Ordered->new( @{ $PAIRS{$size} } );
        }
    );

}

#--------------------------------------------------------------------------#
# Get values
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    my $oh = Hash::Ordered->new( @{ $PAIRS{$size} } );

    my ($v);
    my @keys = keys %{ { @{ $PAIRS{$size} } } };

    my $n = int( .1 * scalar @keys ) || 1;
    my @lookup = map { $keys[ int( rand( scalar @keys ) ) ] } 1 .. $n;

    timeit(
        "Results for fetching ~10% of $size elements",
        sub { $v = $oh->get($_) for @lookup }
    );
}

#--------------------------------------------------------------------------#
# replace values
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    my $oh = Hash::Ordered->new( @{ $PAIRS{$size} } );

    my ($v);
    my @keys = keys %{ { @{ $PAIRS{$size} } } };

    my $n = int( .1 * scalar @keys ) || 1;
    my @lookup = map { $keys[ int( rand( scalar @keys ) ) ] } 1 .. $n;
    my $new_value = irand();

    timeit(
        "Results for replacing ~10% of $size elements",
        sub { $oh->set( $_, $new_value ) for @lookup }
    );
}

#--------------------------------------------------------------------------#
# adding values
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    my ($v);
    my @keys = keys %{ { @{ $PAIRS{$size} } } };

    my $n = int( .1 * scalar @keys ) || 1;

    timeit(
        "Results for adding $size elements to empty hash",
        sub {
            my $oh = Hash::Ordered->new;
            $oh->set( irand(), 42 ) for 1 .. $n;
        }
    );
}

#--------------------------------------------------------------------------#
# delete values
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    my ($v);
    my @keys = keys %{ { @{ $PAIRS{$size} } } };

    my $n = int( .1 * scalar @keys ) || 1;
    my @lookup = map { $keys[ int( rand( scalar @keys ) ) ] } 1 .. $n;

    timeit(
        "Results for creating $size element hash then deleting ~10%",
        sub {
            my $oh = Hash::Ordered->new( @{ $PAIRS{$size} } );
            $oh->delete($_) for @lookup;
        }
    );
}

#--------------------------------------------------------------------------#
# list values
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    my $oh = Hash::Ordered->new( @{ $PAIRS{$size} } );

    my (@list);

    timeit(
        "Results for listing pairs of $size element hash",

        sub { @list = $oh->as_list }
    );

}

