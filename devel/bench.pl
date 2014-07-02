#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;

use lib 'lib';
use Tie::IxHash;
use Tie::LLHash;
use Hash::Ordered;

use Benchmark qw( cmpthese );

my $count = -2;
cmpthese(
    $count,
    {
        'Tie::IxHash'   => \&ix,
        'Tie::LLHash'   => \&ll,
        'Hash::Ordered' => \&oh,
    }
);

use constant SIZE => 20;

sub ix {
    my $h = Tie::IxHash->new;
    $h->STORE( rand(), rand() ) for 1 .. SIZE;
    for my $k ( $h->Keys ) {
        my $val = $h->FETCH($k) if $h->EXISTS($k);
    }
    my @list = map { $_ => $h->FETCH($_) } $h->Keys;
    $h->DELETE($_) for $h->Keys;
}

sub oh {
    my $h = Hash::Ordered->new;
    $h->set( rand(), rand() ) for 1 .. SIZE;
    for my $k ( $h->keys ) {
        my $val = $h->get($k) if $h->exists($k);
    }
    my $list = $h->as_list;
    $h->delete($_) for $h->keys;
}

sub ll {
    tie my %hash, 'Tie::LLHash';
    my $h = tied(%hash);
    $h->last( rand(), rand() ) for 1 .. SIZE;
    for my $k ( keys %hash ) {
        my $val = $hash{$k} if exists $hash{$k};
    }
    my @list = map { $_ => $hash{$_} } keys %hash;
    delete $hash{$_} for keys %hash;
}
