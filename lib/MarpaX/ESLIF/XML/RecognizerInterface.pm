use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::RecognizerInterface;

# ABSTRACT: XML recognizer interface for MarpaX::ESLIF

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module is the XML recognizer interface required by L<MarpaX::ESLIF>.

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML::RecognizerInterface;

    my $recognizerInterface  = MarpaX::ESLIF::XML::RecognizerInterface->new();

=cut

# -----------
# Constructor
# -----------

=head1 SUBROUTINES/METHODS

=head2 new($class, %options)

Instantiate a new recognizer interface object. C<%options> may contain:

=over

=item data

The data to parse

=item encoding

The encoding of the data

=back

=cut

sub new {
    my ($pkg, %options) = @_;
    bless {pos => 0, %options}, $pkg
}

# ----------------
# Required methods
# ----------------

=head2 Required methods

=head3 read($self)

Returns a true or a false value, indicating if last read was successful. Default is a true value.

=cut

sub read                   {        1 } # First read callback will be ok

=head3 isEof($self)

Returns a true or a false value, indicating if end-of-data is reached. Default is a true value.

=cut

sub isEof                  {        $_[0]->{pos} > length($_[0]->{data}) } # ../. and we will say this is EOF

=head3 isCharacterStream($self)

Returns a true or a false value, indicating if last read is a stream of characters. Default is a true value.

=cut

sub isCharacterStream      {        1 } # MarpaX::ESLIF will validate the input

=head3 encoding($self)

Returns encoding information. Default is undef.

=cut

sub encoding               { $_[0]->{encoding} } # Let MarpaX::ESLIF guess eventually

=head3 data($self)

Returns last bunch of data. Default is the string passed in the constructor.

=cut

sub data                   { substr($_[0]->{data}, $_[0]->{pos}++, 1) } # Data itself

=head3 isWithDisableThreshold($self)

Returns a true or a false value, indicating if threshold warning is on or off, respectively. Default is a false value.

=cut

sub isWithDisableThreshold {        0 } # Disable threshold warning ?

=head3 isWithExhaustion($self)

Returns a true or a false value, indicating if exhaustion event is on or off, respectively. Default is a false value.

=cut

sub isWithExhaustion       {        0 } # Exhaustion event ?

=head3 isWithNewline($self)

Returns a true or a false value, indicating if newline count is on or off, respectively. Default is a false value.

=cut

sub isWithNewline          {        1 } # Newline count ?

=head3 isWithTrack($self)

Returns a true or a false value, indicating if absolute position tracking is on or off, respectively. Default is a false value.

=cut

sub isWithTrack            {        1 } # Absolute position tracking ?

=head1 SEE ALSO

L<MarpaX::ESLIF::XML>

=cut

1;
