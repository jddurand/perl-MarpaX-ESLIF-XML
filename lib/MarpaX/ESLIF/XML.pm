use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML;
use MarpaX::ESLIF::XML::XML10;
use MarpaX::ESLIF::XML::XML11;

# ABSTRACT: XML suite using MarpaX::ESLIF

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module is the top fron-end to XML suite using L<MarpaX::ESLIF>.

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML;

    my $eslifxml  = MarpaX::ESLIF::XML->new();
    my $input     = '<?xml></xml>';
    my $xmlhash   = $eslifxml->parse($input);

=cut

1;
