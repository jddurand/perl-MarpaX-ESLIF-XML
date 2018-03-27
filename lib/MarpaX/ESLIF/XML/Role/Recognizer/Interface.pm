use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Role::Recognizer::Interface;
use Role::Tiny;

# ABSTRACT: XML recognizer interface role

# VERSION

# AUTHORITY

=head1 DESCRIPTION

XML recognizer interface role. Requires that the consumer provides C<data> and <isEof> methods.

=cut

=head1 METHODS

=head2 data

A method that returns new data.

=cut

requires 'data';

=head2 isEof

A method that returns a boolean saying if end-of-file is reached.

=cut

requires 'isEof';

=head2 isCharacterStream

Returns a boolean indicating if this is a character stream. Default implementation returns a true value.

=cut

sub isCharacterStream { 1 }

=head2 encoding

Returns a string giving character encoding, C<undef> if none. Default implementation returns C<undef>

=cut

sub encoding { }

=head2 isWithDisableThreshold

Returns a boolean indicating if threshold warning is desirable. Default implementation returns a false value.

=cut

sub isWithDisableThreshold { 0 }

=head2 isWithExhaustion

Returns a boolean indicating if exhaustion event is desirable. Default implementation returns a false value.

=cut

sub isWithExhaustion { 0 }

=head2 isWithNewline

Returns a boolean indicating if newline counting is wanted. Default implementation returns a true value.

=cut

sub isWithNewline { 1 }

=head2 isWithTrack

Returns a boolean indicating if values should be remembered. Default implementation returns a false value.

=cut

sub isWithTrack { 0 }

1;
