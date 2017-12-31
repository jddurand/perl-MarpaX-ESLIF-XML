use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::RecognizerInterface;
use Carp qw/croak/;
use Safe::Isa;

# ABSTRACT: XML Recognizer interface

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module is a MarpaX::ESLIF::Recognizer implementation MarpaX::ESLIF::XML, proxying calls to an external reader.

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML::RecognizerInterface2;

    my $recognizerInterface  = MarpaX::ESLIF::XML::RecognizerInterface2->new();

=cut

# -----------
# Constructor
# -----------

=head1 SUBROUTINES/METHODS

=head2 new($class, %options)

Instantiate a new recognizer interface object. C<%options> keys supported are:

=over

=item reader

Value must be a reader capable of read(), isEof() and data() methods. Required.

=item encode

Encode object. Must be a L<MarpaX::ESLIF::XML::Encode> instance. Required.

=item isWithExhaustion

Flag saying if exhaustion is permitted. Optional and defaults to a false value.

=item isCharacterStream

Flag to forced character stream nature, regardless if the encode layer is doing encode/decode. This mean that we will let MarpaX::ESLIF to a charset detection when the encode object is a no-op.

=item isWithNewline

Flag saying if newline count is permitted. Optional and defaults to a false value.

=item isWithTrack

If set to a true value, forces ESLIF to keep track of all symbols locations. Defaults to a false value.

=back

=cut

sub new {
    my ($class, %options) = @_;

    my $reader = $options{reader} // croak "Missing reader option";
    map { croak "reader must implement the $_() method" unless $reader->$_can($_) } qw/data isEof read/;

    my $encode = $options{encode} // croak "Missing encode option";
    croak 'encode must be an MarpaX::ESLIF::XML::Encode instance' unless $encode->$_isa('MarpaX::ESLIF::XML::Encode');

    my $isCharacterStream = $options{isCharacterStream} // $encode->{isCharacterStream};
    my $isWithNewline     = $options{isWithNewline} // 0;
    my $isWithExhaustion  = $options{isWithExhaustion} // 0;
    my $isWithTrack       = $options{isWithTrack} // 0;

    return bless {
        reader            => $reader,
        encode            => $encode,
        isCharacterStream => $isCharacterStream,
        isWithNewline     => $isWithNewline,
        isWithExhaustion  => $isWithExhaustion,
        isWithTrack       => $isWithTrack
    }, $class
}

# ------------------------------------------
# Required MarpaX::ESLIF::Recognizer methods
# ------------------------------------------

=head2 Required methods

=head3 read($self)

Returns a true or a false value, indicating if last read was successful.

=cut

sub read {
    my ($self) = @_;
    #
    # Because the encoding object makes lookahead happening, we can
    # very well be already at the eof. We should not call the read()
    # method in this case.
    #
    if ($self->{reader}->read) {
        return 1
    } else {
        #
        # This is fatal unless eof is reached and the encoding object
        # was never consumed while it has cached data
        #
        return $self->{encode}->have_initial_data
    }
}

=head3 isEof($self)

Returns a true or a false value, indicating if end-of-data is reached.

=cut

sub isEof {
    my ($self) = @_;

    return $self->{encode}->have_initial_data || $self->{reader}->isEof;
}

=head3 encoding($self)

Returns encoding information.

=cut

sub encoding {
    my ($self) = @_;

    return $self->{encode}->to
}

=head3 isCharacterStream($self)

Returns a true or a false value, indicating if last read is a stream of characters.

=cut

sub isCharacterStream {
    my ($self) = @_;

    return $self->{isCharacterStream}
}

=head3 data($self)

Returns last bunch of data, eventually encoded.

=cut

sub data {
    my ($self) = @_;

    return $self->{encode}->from_to($self->{reader}->data)
}

=head3 isWithDisableThreshold($self)

Returns a true or a false value, indicating if threshold warning is on or off, respectively.

=cut

sub isWithDisableThreshold {
    return 0
}

=head3 isWithExhaustion($self)

Returns a true or a false value, indicating if exhaustion event is on or off, respectively.

=cut

sub isWithExhaustion {
    my ($self) = @_;

    return $self->{isWithExhaustion}
}

=head3 isWithNewline($self)

Returns a true or a false value, indicating if newline count is on or off, respectively.

=cut

sub isWithNewline {
    my ($self) = @_;

    return $self->{isWithNewline}
}

=head3 isWithTrack($self)

Returns a true or a false value, indicating if absolute position tracking is on or off, respectively.

=cut

sub isWithTrack {
    my ($self) = @_;

    return $self->{isWithTrack}
}

=head1 SEE ALSO

L<MarpaX::ESLIF::XML::Recognizer>

=cut

1;
