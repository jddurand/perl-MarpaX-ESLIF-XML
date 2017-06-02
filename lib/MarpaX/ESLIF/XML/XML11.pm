use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::XML11;

# ABSTRACT: XML 1.1 suite using MarpaX::ESLIF

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module is the XML 1.1 implementation L<MarpaX::ESLIF::XML>.

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML11;

    my $eslifxml11 = MarpaX::ESLIF::XML::XML11->new();
    my $input      = '<?xml></xml>';
    my $xmlhash    = $eslifxml11->parse($input);

=cut

1;
