use 5.006;
use strict;
use warnings;

package Hash::Ordered;
# ABSTRACT: A compact, pure-Perl ordered hash class
# VERSION

use Carp ();

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

sub keys {
    my ($self) = @_;
    return @{ $self->[_KEYS] };
}

sub values {
    my ($self) = @_;
    return map { $self->[_DATA]{$_} } @{ $self->[_KEYS] };
}

1;

=for Pod::Coverage BUILD

=head1 SYNOPSIS

    use Hash::Ordered;

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
