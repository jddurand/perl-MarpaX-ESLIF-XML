use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::RecognizerInterface;
use Carp qw/croak/;
use Data::HexDump;
use Encode qw/decode encode find_encoding/;
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

=item encoder

Encoder object. If set, it must be a L<MarpaX::ESLIF::XML::Encode> instance, character stream is then enabled and callback required method is read(). If not set binary stream is enabled and callback required method is byte(). Default is not set.

=item reader

Value must be a reader capable of byte(), isEof() and data() methods. Required.

=item exhaustion

Flag saying if exhaustion is permitted. Optional and defaults to a false value.

=item newline

Flag saying if newline count is permitted. Optional and defaults to a false value.

=item prev_bookkeeping

Previous bookkeeping. Optional.

=item prev_bookkeeping_is_character

If true, prev_bookkeeping is assumed to be composed of characters as per the C<from> method of C<encoder>. It is invalid to set this flag to a true value and to not have the C<encoder> setting. Default is a false value.

=item isCharacterStream

If set to a true value, forces character stream even if there is no encoding. Then MarpaX::ESLIF will try to do itself character detection. Defaults to the existence of encoding.

=item isWithTrack

If set to a true value, forces ESLIF to keep track of all symbols locations. Defaults to a false value.

=item remember

Remember all data. If true, all read data is remembered. If encoding is set, the it is characters in that encoding, else it is binary. Optional and defaults to a false value.

=back

=cut

#
# To save time
#
my %ENC;

sub _get_enc {
    my ($self, $encname) = @_;

    return $ENC{$encname} //= find_encoding($encname);
}

sub new {
    my ($class, %options) = @_;

    my $encoding                   = $options{encoding};
    my $reader                     = $options{reader} // croak "Missing reader option";
    my $prev_bookkeeping           = $options{prev_bookkeeping};
    my $prev_bookkeeping_is_utf8   = $options{prev_bookkeeping_is_utf8};
    my $remember                   = $options{remember} // 0;
    my $exhaustion                 = $options{exhaustion} // 0;
    my $newline                    = $options{newline} // 0;
    my $isCharacterStream          = $options{isCharacterStream} // defined($encoding);
    my $isWithTrack                = $options{isWithTrack} // 0;

    croak 'prev_bookkeeping_is_utf8 is invalid without encoding setting' if ($prev_bookkeeping_is_utf8 && ! $encoding);

    if (defined($encoding)) {
        map { croak "reader must implement the $_() method" unless $reader->$_can($_) } qw/data isEof read/
    } else {
        map { croak "reader must implement the $_() method" unless $reader->$_can($_) } qw/data isEof byte/
    }

    $log->tracef("Recognizer initialized {encoding=%s, isCharacterStream=%s, reader=%s, prev_bookkeeping_is_utf8=%s, prev_bookkeeping of %s bytes, remember=%s, exhaustion=%s, newline=%s}", $encoding, $isCharacterStream, "$reader", $prev_bookkeeping_is_utf8, length($prev_bookkeeping), $remember, $exhaustion, $newline);

    return bless {encoding                 => $encoding,
                  reader                   => $reader,
                  prev_bookkeeping         => $prev_bookkeeping,
                  prev_bookkeeping_is_utf8 => $prev_bookkeeping_is_utf8,
                  remember                 => $remember,
                  newline                  => $newline,
                  read_status              => 0,
                  isCharacterStream        => $isCharacterStream,
                  isWithExhaustion         => defined($exhaustion),
                  isWithNewline            => defined($newline),
                  isWithTrack              => $isWithTrack,
                  decodeBuffer             => undef,
                  encodeBuffer             => undef,
                  bookkeeping              => ''}, $class
}

=head3 bookkeeping($self, $data)

Returns all data that is read, always equal to an empty string unless recognizer was created with a true C<remember> flag. Eventually sets bookkeeping if C<$data> is defined.

=cut

sub bookkeeping {
    my ($self, $data) = @_;

    if (defined($data)) {
        return $self->{bookkeeping} = $data
    } else {
        return $self->{bookkeeping}
    }
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
    $rc = defined($self->{prev_bookkeeping}) ? 1 : $self->{read_status};
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

Returns encoding information. Default is UTF-8 if input has an associated encoding.

=cut

sub encoding {
    my ($self) = @_;

    my $rc = $self->{encoding} ? 'UTF-8' : undef;   # If encoding is set, we do ourself conversion to UTF-8
    $log->tracef("encoding ? %s", $rc);
    return $rc
}

=head3 input_encoding($self)

Returns input encoding information. Default is value from BOM or guess.

=cut

sub input_encoding {
    my ($self) = @_;

    my $rc = $self->{encoding};
    $log->tracef("input encoding ? %s", $rc);
    return $rc
}

=head3 data($self)

Returns last bunch of data.

=cut

sub _binarystring {
    my ($self, $data) = @_;

    return pack('C*', unpack('C*', $data));
}

sub data {
    my ($self) = @_;

    $log->trace("data ?");

    my $binarystring;

    if ($log->is_trace) {
        if (defined($self->{prev_bookkeeping})) {
            if ($self->{prev_bookkeeping_is_utf8}) {
                $log->tracef("Bookkeeping in UTF-8:\n%s", HexDump($self->{prev_bookkeeping}));
            } else {
                $log->tracef("Bookkeeping in binary:\n%s", HexDump($self->{prev_bookkeeping}));
            }
        } else {
            $log->trace("No bookkeeping");
        }
    }
    if (defined($self->{prev_bookkeeping}) && ! $self->{prev_bookkeeping_is_utf8}) {
        $binarystring = $self->_binarystring($self->{prev_bookkeeping});
        $self->{prev_bookkeeping} = undef;
    }
    $binarystring .= $self->_binarystring($self->{reader}->data) if $self->{read_status};

    #
    # Very important point: when we say to ESLIF that data is in UTF-8, it
    # will "trust" user input, that is it will NOT call for character conversion.
    # At most it will do UTF-8 validation using PCRE2 built-in.
    #
    # So, why not letting ESLIF do conversion? It is using tconv: behind tconv there is:
    # - Either ICU, which is perfectly ok, but not use when distributed with CPAN
    # - Either system's iconv, which is a catastrophy
    # - Either Windows's API, which is quite ok
    #
    # In higher languages, all pitfalls of encoding are already handled, so when it is
    # possible it is recommended to use the higher-level language facilities.
    #
    # If character stream is set but no encoding is provided, then we let anyway
    # MarpaX::ESLIF guess (since ICU is not within the CPAN version, it will use cchardet).
    #
    my $rc;

    if (defined($binarystring)) {
        if ($self->{encoding}) {
            if ($log->is_trace) {
                $log->tracef("Data claimed to be in %s charset:\n%s", $self->{encoding}, HexDump($binarystring))
            }
            $self->{decodeBuffer} .= $binarystring;
            my $decoded = $self->_get_enc($self->{encoding})->decode($self->{decodeBuffer}, Encode::FB_QUIET);
            $self->{encodeBuffer} .= $decoded;
            my $encoded = $self->_get_enc('UTF-8')->encode($self->{encodeBuffer}, Encode::FB_QUIET);

            my $characterdata;
            if (defined($self->{prev_bookkeeping}) && $self->{prev_bookkeeping_is_utf8}) {
                my $bookkeeping_length = bytes::length($self->{prev_bookkeeping});
                $characterdata = $self->{prev_bookkeeping};
                $self->{prev_bookkeeping} = undef;
                $characterdata .= $encoded;
                if ($log->is_trace) {
                    $log->tracef("Data claimed to be in %s charset and translated to UTF-8, prepended with bookeeping bytes:\n%s", $self->{encoding}, HexDump($characterdata), $bookkeeping_length);
                }
            } else {
                $characterdata = $encoded;
                if ($log->is_trace) {
                    $log->tracef("Data claimed to be in %s charset and translated to UTF-8:\n%s", $self->{encoding}, HexDump($characterdata));
                }
            }

            $rc = $characterdata;
        } else {
            #
            # Constructor made sure that it is not possible to have a prev_bookkeeping_is_utf8 without encoding.
            # The other case is handle at the beginning of this method.
            #
            if ($log->is_trace) {
                $log->tracef("Data claimed to be binary:\n%s", HexDump($binarystring))
            }

            $rc = $binarystring;
        }
    
        $self->{bookkeeping} .= $rc if $self->{remember};
    }
    
    return $rc
}

=head3 isWithDisableThreshold($self)

Returns a true or a false value, indicating if threshold warning is on or off, respectively. Default is a false value.

=cut

sub isWithDisableThreshold {
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
    my ($self) = @_;

    $log->tracef("track ? %s", $self->{isWithTrack});

    return $self->{isWithTrack}
}

=head1 SEE ALSO

L<MarpaX::ESLIF::XML::Recognizer>

=cut

1;
