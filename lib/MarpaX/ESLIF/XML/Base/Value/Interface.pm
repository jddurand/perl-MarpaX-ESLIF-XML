use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Base::Value::Interface;

# ABSTRACT: MarpaX::ESLIF::XML Value Interface

# VERSION

# AUTHORITY

=head1 DESCRIPTION

MarpaX::ESLIF::XML's Value Interface

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML::Base::Value::Interface;

    my $valueInterface = MarpaX::ESLIF::XML::Base::Value::Interface->new();

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

=head1 SEE ALSO

L<MarpaX::ESLIF::XML>

=cut

sub defaultRuleAction {
    my $self = shift;

    my $what = $MarpaX::ESLIF::Context::ruleName // $MarpaX::ESLIF::Context::symbolName;
    return { $what => \@_ }
}

sub defaultSymbolAction {
    my $self = shift;

    my $what = $MarpaX::ESLIF::Context::ruleName // $MarpaX::ESLIF::Context::symbolName;
    return { $what => \@_ }
}

1;
