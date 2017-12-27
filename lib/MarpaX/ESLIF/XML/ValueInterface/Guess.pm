use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::ValueInterface::Guess;

# ABSTRACT: MarpaX::ESLIF::XML Value Interface for Guess

# VERSION

# AUTHORITY

=head1 DESCRIPTION

MarpaX::ESLIF::XML's Value Interface for Guess

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML::ValueInterface::Guess;

    my $valueInterface = MarpaX::ESLIF::XML::ValueInterface::Guess->new();

=cut

# -----------
# Constructor
# -----------

=head1 SUBROUTINES/METHODS

=head2 new($class)

Instantiate a new value interface object.

=cut

sub new {
    my ($class) = @_;

    return bless { result => undef }, $class
}

# ----------------
# Required methods
# ----------------

=head2 Required methods

=head3 isWithHighRankOnly

Returns a true or a false value, indicating if valuation should use highest ranked rules or not, respectively. Default is a true value.

=cut

sub isWithHighRankOnly {
    return 1
}

=head3 isWithOrderByRank

Returns a true or a false value, indicating if valuation should order by rule rank or not, respectively. Default is a true value.

=cut

sub isWithOrderByRank {
    return 1
}

=head3 isWithAmbiguous

Returns a true or a false value, indicating if valuation should allow ambiguous parse tree or not, respectively. Default is a false value.

=cut

sub isWithAmbiguous {
    return 0
}

=head3 isWithNull

Returns a true or a false value, indicating if valuation should allow a null parse tree or not, respectively. Default is a false value.

=cut

sub isWithNull {
    return 0
}

=head3 maxParses

Returns the number of maximum parse tree valuations. Default is unlimited (i.e. a false value).

=cut

sub maxParses {
    return 0
}

=head3 getResult

Returns the current parse tree value.

=cut

sub getResult {
    my ($self) = @_;

    return $self->{result}
}

=head3 setResult

Sets the current parse tree value.

=cut

sub setResult {
    my ($self) = @_;

    return $self->{result} = $_[1]
}

=head3 UTF_32BE($self)

UTF_32BE action. Returns the array reference [ 'UTF-32BE', 4].

=cut

sub UTF_32BE {
    return [ 'UTF-32BE', 4 ]
}

=head3 UTF_32LE($self)

UTF_32LE action. Returns the array reference [ 'UTF-32LE', 4 ].

=cut

sub UTF_32LE {
    return [ 'UTF-32LE', 4 ]
}

=head3 UTF_16BE($self)

UTF_16BE action. Returns the array reference [ 'UTF-16BE', 4 ].

=cut

sub UTF_16BE {
    return [ 'UTF-16BE', 4 ]
}

=head3 UTF_16LE($self)

UTF_16LE action. Returns the array reference [ 'UTF-16LE', 4 ].

=cut

sub UTF_16LE {
    return [ 'UTF-16LE', 4 ]
}

=head3 UTF_8($self)

UTF_8 action. Returns the array reference [ 'UTF-8', 4 ].

=cut

sub UTF_8 {
    return [ 'UTF-8', 4 ]
}

=head3 EBCDIC($self)

EBCDIC action. Returns the array reference [ 'EBCDIC', 4 ].

=cut

sub EBCDIC {
    return [ 'EBCDIC', 4 ]
}

=head1 SEE ALSO

L<MarpaX::ESLIF::XML>

=cut

1;
