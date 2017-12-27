use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::RecognizerInterface;
use Carp qw/croak/;
use Data::HexDump;
use Log::Any qw/$log/;
use Safe::Isa;

# ABSTRACT: XML Recognizer interface for MarpaX::ESLIF

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module is a recognizer that handles all cases for charset recognition from BOM, guess, XML declaration, and XML grammar itself.

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML::RecognizerInterface;

    my $recognizerInterface  = MarpaX::ESLIF::XML::RecognizerInterface->new();

=cut

# -----------
# Constructor
# -----------

=head1 SUBROUTINES/METHODS

=head2 new($class, %options)

Instantiate a new recognizer interface object. C<%options> keys supported are:

=over

=item encoding

Encoding. If set, then character stream is enabled and callback required method is read(), else binary stream is enabled and callback required method is byte(). Required.

=item reader

Value must be a reader capable of byte(), isEof() and data() methods. Required.

=item exhaustion

Flag saying if exhaustion is permitted. Optional and defaults to a false value.

=item newline

Flag saying if newline count is permitted. Optional and defaults to a false value.

=item prev_bookkeeping

Previous bookkeeping. Optional and defaults to the empty string.

=item isCharacterStream

If set to a true value, forces character stream even if there is no encoding. Then MarpaX::ESLIF will try to do itself character detection.

=item remember

Remember all data. If true, all read data is remembered. Optional and defaults to a false value.

=back

=cut

sub new {
    my ($class, %options) = @_;

    my $encoding          = $options{encoding};
    my $reader            = $options{reader} // croak "Missing reader option";
    my $prev_bookkeeping  = $options{prev_bookkeeping} // '';
    my $remember          = $options{remember} // 0;
    my $exhaustion        = $options{exhaustion} // 0;
    my $newline           = $options{newline} // 0;
    my $isCharacterStream = $options{isCharacterStream} // defined($encoding);

    if (defined($encoding)) {
        map { croak "reader must implement the $_() method" unless $reader->$_can($_) } qw/data isEof read/
    } else {
        map { croak "reader must implement the $_() method" unless $reader->$_can($_) } qw/data isEof byte/
    }

    $log->tracef("Recognizer initialized {encoding=%s, isCharacterStream=%s, reader=%s, prev_bookkeeping of %d bytes, remember=%s, exhaustion=%s, newline=%s}", $encoding, $isCharacterStream, "$reader", length($prev_bookkeeping), $remember, $exhaustion, $newline);

    return bless {encoding          => $encoding,
                  reader            => $reader,
                  prev_bookkeeping  => $prev_bookkeeping,
                  remember          => $remember,
                  newline           => $newline,
                  read_status       => 0,
                  isCharacterStream => $isCharacterStream,
                  isWithExhaustion  => defined($exhaustion),
                  isWithNewline     => defined($newline),
                  bookkeeping       => ''}, $class
}

=head3 bookkeeping($self)

Returns all data that is read, always equal to an empty string unless recognizer was created with a true C<remember> flag.

=cut

sub bookkeeping {
    my ($self) = @_;

    return $self->{bookkeeping}
}

# ----------------
# Required methods
# ----------------

=head2 Required methods

=head3 read($self)

Returns a true or a false value, indicating if last read was successful. Default is a true value.

=cut

sub read {
    my ($self) = @_;

    my $rc;

    $self->{read_status} = $self->{isCharacterStream} ? $self->{reader}->read : $self->{reader}->byte;
    #
    # We always return a true value if there is an initial (aka previous) bookkeeping
    #
    $rc = length($self->{prev_bookkeeping}) ? 1 : $self->{read_status};
    $log->tracef("read ? %s", $rc);

    return $rc
}

=head3 isEof($self)

Returns a true or a false value, indicating if end-of-data is reached. Default is a true value.

=cut

sub isEof {
    my ($self) = @_;

    my $rc = $self->{reader}->isEof;
    $log->tracef("eof ? %s", $rc);
    return $rc
}

=head3 isCharacterStream($self)

Returns a true or a false value, indicating if last read is a stream of characters. Default is a true value.

=cut

sub isCharacterStream {
    my ($self) = @_;

    $log->tracef("character stream ? %s", $self->{isCharacterStream});
    return $self->{isCharacterStream}
}

=head3 encoding($self)

Returns encoding information. Default is value from BOM or guess.

=cut

sub encoding {
    my ($self) = @_;

    $log->tracef("encoding ? %s", $self->{encoding});
    return $self->{encoding}
}

=head3 data($self)

Returns last bunch of data.

=cut

sub data {
    my ($self) = @_;

    $log->trace("data ?");

    my $data;

    $data .= $self->{prev_bookkeeping} if length($self->{prev_bookkeeping});
    $data .= $self->{reader}->data if $self->{read_status};

    $self->{prev_bookkeeping} = '';
    $self->{bookkeeping} .= $data if $self->{remember};

    if ($log->is_trace) {
        $log->tracef("\n%s", HexDump($data))
    }

    return $data
}

=head3 isWithDisableThreshold($self)

Returns a true or a false value, indicating if threshold warning is on or off, respectively. Default is a false value.

=cut

sub isWithDisableThreshold {

    $log->trace("disable threshold ? 0");

    return 0
}

=head3 isWithExhaustion($self)

Returns a true or a false value, indicating if exhaustion event is on or off, respectively. Default is a true value.

=cut

sub isWithExhaustion {
    my ($self) = @_;

    $log->tracef("exhaustion ? %s", $self->{isWithExhaustion});

    return $self->{isWithExhaustion}
}

=head3 isWithNewline($self)

Returns a true or a false value, indicating if newline count is on or off, respectively. Default is a false value.

=cut

sub isWithNewline {
    my ($self) = @_;

    $log->tracef("newline ? %s", $self->{isWithNewline});

    return $self->{isWithNewline}
}

=head3 isWithTrack($self)

Returns a true or a false value, indicating if absolute position tracking is on or off, respectively. Default is a false value.

=cut

sub isWithTrack {
    $log->trace("track ? 0");

    return 0
}

=head1 SEE ALSO

L<MarpaX::ESLIF::XML::Recognizer>

=cut

1;
