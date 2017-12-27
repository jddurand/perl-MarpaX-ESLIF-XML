use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML;
use Carp qw/croak/;

# ABSTRACT: XML suite using MarpaX::ESLIF

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module is the top front-end to XML suite using L<MarpaX::ESLIF>.

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML;

    #
    # $reader must be an object that support the read() and close() methods.
    # MarpaX::ESLIF::XML::Reader provides subclasses for the major
    # types of input.
    #
    my $eslifxml  = MarpaX::ESLIF::XML->new(logger => $logger, reader => $reader);
    my $xmlhash   = $eslifxml->parse();

=cut

sub new {
    my ($pkg, %options) = @_;

    my $logger = delete($options{logger});
    my $eslif = MarpaX::ESLIF->new($logger); # This is a multiton

    return bless {
        grammar_xmldecl => MarpaX::ESLIF::Grammar->new($eslif, $XMLDecl),
        grammar         => MarpaX::ESLIF::Grammar->new($eslif, $XML),
        %options
    }, $pkg
}

1;
