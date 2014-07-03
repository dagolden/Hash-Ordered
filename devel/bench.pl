#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;

use lib 'lib';
use List::Util qw/shuffle/;
use Math::Random::MT::Auto qw/irand/;
use Benchmark qw( countit );

use Hash::Ordered;
use Tie::IxHash;
use Tie::LLHash;
use Tie::Hash::Indexed;
use Array::AsHash;
use Array::OrdHash;
use Data::XHash;

use constant COUNT => 5;
use constant NUMS => [ 10, 100, 1000 ];

STDOUT->autoflush(1);

my %PAIRS = (
    map {
        $_ => [ map { irand() => irand() } 1 .. $_ ]
    } @{ NUMS() }
);

sub time_them {
    my (%mark) = @_;
    my %results;

    for my $k ( sort keys %mark ) {
##        warn "Timing $k...\n";
        my $res = countit( COUNT, $mark{$k} );
        my $iter_s = $res->iters / ( $res->cpu_a + 1e-9 );
        $results{$k} = $iter_s;
    }

    printf( "%20s %d/s\n", $_, $results{$_} )
      for sort { $results{$b} <=> $results{$a} } keys %results;

    say "";
}

#--------------------------------------------------------------------------#
# Create with keys
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    say my $title = "Results for ordered hash creation for $size elements";

    my %mark;

    $mark{"h:o"} = sub {
        my $h = Hash::Ordered->new( @{ $PAIRS{$size} } );
    };

    $mark{"t:ix_oo"} = sub {
        my $h = Tie::IxHash->new( @{ $PAIRS{$size} } );
    };

    $mark{"t:ix_th"} = sub {
        tie my %h, 'Tie::IxHash', @{ $PAIRS{$size} };
    };

    $mark{"t:llh"} = sub {
        tie my %h, 'Tie::LLHash', @{ $PAIRS{$size} };
    };

    $mark{"t:h:i"} = sub {
        tie my %h, 'Tie::Hash::Indexed', @{ $PAIRS{$size} };
    };

    $mark{"a:ah_rf"} = sub {
        my $h = Array::AsHash->new( { array => [ @{ $PAIRS{$size} } ] } );
    };

    $mark{"a:ah_cp"} = sub {
        my $h = Array::AsHash->new( { array => $PAIRS{$size}, clone => 1 } );
    };

    $mark{"d:xh_ls"} = sub {
        my $h = Data::XHash::xh( @{ $PAIRS{$size} } );
    };

    $mark{"d:xh_rf"} = sub {
        my $h = Data::XHash::xhr( [ @{ $PAIRS{$size} } ] );
    };

    $mark{"a:oh"} = sub {
        my $h = Array::OrdHash->new( @{ $PAIRS{$size} } );
    };

    time_them(%mark);
}

#--------------------------------------------------------------------------#
# Get values
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    say my $title = "Results for fetching ~10% of $size elements";

    my $oh     = Hash::Ordered->new( @{ $PAIRS{$size} } );
    my $tix_oo = Tie::IxHash->new( @{ $PAIRS{$size} } );
    my $aah    = Array::AsHash->new( { array => [ @{ $PAIRS{$size} } ] } );
    my $dxh    = Data::XHash::xh( @{ $PAIRS{$size} } );
    my $aoh    = Array::OrdHash->new( @{ $PAIRS{$size} } );
    tie my %tix_th, 'Tie::IxHash',        @{ $PAIRS{$size} };
    tie my %tllh,   'Tie::LLHash',        @{ $PAIRS{$size} };
    tie my %thi,    'Tie::Hash::Indexed', @{ $PAIRS{$size} };

    my ( %mark, $v );
    my @keys = keys %{ { @{ $PAIRS{$size} } } };

    my $n = int( .1 * scalar @keys ) || 1;
    my @lookup = map { $keys[ int( rand( scalar @keys ) ) ] } 1 .. $n;

    $mark{"h:o"}     = sub { $v = $oh->get($_)       for @lookup };
    $mark{"t:ix_oo"} = sub { $v = $tix_oo->FETCH($_) for @lookup };
    $mark{"t:ix_th"} = sub { $v = $tix_th{$_}        for @lookup };
    $mark{"t:llh"}   = sub { $v = $tllh{$_}          for @lookup };
    $mark{"t:h:i"}   = sub { $v = $thi{$_}           for @lookup };
    $mark{"a:ah"}    = sub { $v = $aah->get($_)      for @lookup };
    $mark{"d:xh_oo"} = sub { $v = $dxh->fetch($_)    for @lookup };
    $mark{"d:xh_rf"} = sub { $v = $dxh->{$_}         for @lookup };
    $mark{"a:oh"}    = sub { $v = $aoh->{$_}         for @lookup };

    time_them(%mark);
}

#--------------------------------------------------------------------------#
# replace values
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    say my $title = "Results for replacing ~10% of $size elements";

    my $oh     = Hash::Ordered->new( @{ $PAIRS{$size} } );
    my $tix_oo = Tie::IxHash->new( @{ $PAIRS{$size} } );
    my $aah    = Array::AsHash->new( { array => [ @{ $PAIRS{$size} } ] } );
    my $dxh    = Data::XHash::xh( @{ $PAIRS{$size} } );
    my $aoh    = Array::OrdHash->new( @{ $PAIRS{$size} } );
    tie my %tix_th, 'Tie::IxHash',        @{ $PAIRS{$size} };
    tie my %tllh,   'Tie::LLHash',        @{ $PAIRS{$size} };
    tie my %thi,    'Tie::Hash::Indexed', @{ $PAIRS{$size} };

    my ( %mark, $v );
    my @keys = keys %{ { @{ $PAIRS{$size} } } };

    my $n = int( .1 * scalar @keys ) || 1;
    my @lookup = map { $keys[ int( rand( scalar @keys ) ) ] } 1 .. $n;
    my $new_value = irand();

    $mark{"h:o"} = sub { $oh->set( $_, $new_value ) for @lookup };
    $mark{"t:ix_oo"} = sub { $tix_oo->STORE( $_, $new_value ) for @lookup };
    $mark{"t:ix_th"} = sub { $tix_th{$_} = $new_value for @lookup };
    $mark{"t:llh"}   = sub { $tllh{$_}   = $new_value for @lookup };
    $mark{"t:h:i"}   = sub { $thi{$_}    = $new_value for @lookup };
    $mark{"a:ah"} = sub { $aah->put( $_, $new_value ) for @lookup };
    $mark{"d:xh_oo"} = sub { $dxh->store( $_, $new_value ) for @lookup };
    $mark{"d:xh_rf"} = sub { $dxh->{$_} = $new_value for @lookup };
    $mark{"a:oh"}    = sub { $aoh->{$_} = $new_value for @lookup };

    time_them(%mark);
}

#--------------------------------------------------------------------------#
# adding values
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    say my $title = "Results for adding $size elements to empty hash";

    my ( %mark, $v );
    my @keys = keys %{ { @{ $PAIRS{$size} } } };

    my $n = int( .1 * scalar @keys ) || 1;

    $mark{"h:o"} = sub {
        my $oh = Hash::Ordered->new;
        $oh->set( irand(), 42 ) for 1 .. $n;
    };

    $mark{"t:ix_oo"} = sub {
        my $tix_oo = Tie::IxHash->new();
        $tix_oo->STORE( irand(), 42 ) for 1 .. $n;
    };

    $mark{"t:ix_th"} = sub {
        tie my %tix_th, 'Tie::IxHash';
        $tix_th{ irand() } = 42 for 1 .. $n

    };

    $mark{"t:llh"} = sub {
        tie my %tllh, 'Tie::LLHash';
        tied(%tllh)->last( irand(), 42 ) for 1 .. $n;
    };

    $mark{"t:h:i"} = sub {
        tie my %thi, 'Tie::Hash::Indexed';
        $thi{ irand() } = 42 for 1 .. $n;
    };

    $mark{"a:ah"} = sub {
        my $aah = Array::AsHash->new();
        $aah->put( irand(), 42 ) for 1 .. $n

    };

    $mark{"d:xh_oo"} = sub {
        my $dxh = Data::XHash::xh();
        $dxh->store( irand(), 42 ) for 1 .. $n

    };

    $mark{"d:xh_rf"} = sub {
        my $dxh = Data::XHash::xh();
        $dxh->{ irand() } = 42 for 1 .. $n

    };

    $mark{"a:oh"} = sub {
        my $aoh = Array::OrdHash->new();
        push @$aoh, irand(), 42 for 1 .. $n

    };

    time_them(%mark);
}

#--------------------------------------------------------------------------#
# delete values
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    say my $title = "Results for creating $size element hash then deleting ~10%";

    my ( %mark, $v );
    my @keys = keys %{ { @{ $PAIRS{$size} } } };

    my $n = int( .1 * scalar @keys ) || 1;
    my @lookup = map { $keys[ int( rand( scalar @keys ) ) ] } 1 .. $n;

    $mark{"h:o"} = sub {
        my $oh = Hash::Ordered->new( @{ $PAIRS{$size} } );
        $oh->delete($_) for @lookup;
    };

    $mark{"t:ix_oo"} = sub {
        my $tix_oo = Tie::IxHash->new( @{ $PAIRS{$size} } );
        $tix_oo->DELETE($_) for @lookup;
    };

    $mark{"t:ix_th"} = sub {
        tie my %tix_th, 'Tie::IxHash', @{ $PAIRS{$size} };
        delete $tix_th{$_} for @lookup;
    };

    $mark{"t:llh"} = sub {
        tie my %tllh, 'Tie::LLHash', @{ $PAIRS{$size} };
        delete $tllh{$_} for @lookup;
    };

    $mark{"t:h:i"} = sub {
        tie my %thi, 'Tie::Hash::Indexed', @{ $PAIRS{$size} };
        delete $thi{$_} for @lookup;
    };

    $mark{"a:ah"} = sub {
        my $aah = Array::AsHash->new( { array => [ @{ $PAIRS{$size} } ] } );
        $aah->delete($_) for @lookup;
    };

    $mark{"d:xh_oo"} = sub {
        my $dxh = Data::XHash::xh( @{ $PAIRS{$size} } );
        $dxh->delete($_) for @lookup;
    };

    $mark{"d:xh_rf"} = sub {
        my $dxh = Data::XHash::xh( @{ $PAIRS{$size} } );
        delete $dxh->{$_} for @lookup;
    };

    $mark{"a:oh"} = sub {
        my $aoh = Array::OrdHash->new( @{ $PAIRS{$size} } );
        delete $aoh->{$_} for @lookup;
    };

    time_them(%mark);
}

#--------------------------------------------------------------------------#
# list values
#--------------------------------------------------------------------------#

for my $size ( @{ NUMS() } ) {

    say my $title = "Results for listing pairs of $size element hash";

    my $oh     = Hash::Ordered->new( @{ $PAIRS{$size} } );
    my $tix_oo = Tie::IxHash->new( @{ $PAIRS{$size} } );
    my $aah    = Array::AsHash->new( { array => [ @{ $PAIRS{$size} } ] } );
    my $dxh    = Data::XHash::xh( @{ $PAIRS{$size} } );
    my $aoh    = Array::OrdHash->new( @{ $PAIRS{$size} } );
    tie my %tix_th, 'Tie::IxHash',        @{ $PAIRS{$size} };
    tie my %tllh,   'Tie::LLHash',        @{ $PAIRS{$size} };
    tie my %thi,    'Tie::Hash::Indexed', @{ $PAIRS{$size} };

    my ( %mark, @list );

    $mark{"h:o"} = sub { @list = $oh->as_list };
    $mark{"t:ix_oo"} = sub {
        @list = map { $_ => $tix_oo->FETCH($_) } $tix_oo->Keys;
    };
    $mark{"t:ix_th"} = sub { @list = %tix_th };
    $mark{"t:llh"}   = sub { @list = %tllh };
    $mark{"t:h:i"}   = sub { @list = %thi };
    $mark{"a:ah"}    = sub { @list = $aah->get_array };
    $mark{"d:xh"}    = sub { @list = $dxh->as_array };
    $mark{"a:oh"}    = sub { @list = %$aoh };

    time_them(%mark);
}

