use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Encode;
use Carp qw/croak/;
# use Data::HexDump qw/HexDump/;
use Encode qw/find_encoding/;
# use Log::Any qw/$log/;


# ABSTRACT: Encode helper for MarpaX::ESLIF::XML

# VERSION

# AUTHORITY

=head1 DESCRIPTION

When possible, it is recommended to use Higher-Level language to take care of encoding. Otherwise MarpaX::ESLIF will I<guess> and this is not 100% foolprool. In addition, the module delivered with CPAN is using only iconv (or the native Windows's API, if on this platform), which is not as powerful as perl's builtin language support.

Without from and to encodings, this module is a no-op, except for eventual bookkeeping.

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML::Encode;

    my $recognizerInterface  = MarpaX::ESLIF::XML::Encode->new();

=cut

# -----------
# Constructor
# -----------

=head1 SUBROUTINES/METHODS

=head2 new($class, %options)

Instantiates a new object. C<%options> keys supported are:

=over

=item from

From encoding. If set, it must be resolved by the L<Encode> module. Optional.

=item from_init

Initial octets in the C<from> encoding. Defaults to undef.

=item from_remember

If set to a true value, will remember all data in the C<from> encoding. Default is a false value.

=item to

To encoding. If set, it must be resolved by the L<Encode> module. Optional.

=item to_init

Initial octets in the C<to> encoding. Defaults to undef.

=item to_remember

If set to a true value, will remember all data in the C<from> encoding. Default is a false value.

=back

=cut

#
# To save time
#
my %ENC;
sub _get_enc {
    my ($encname) = @_;

    $ENC{$encname} //= find_encoding($encname);

    return $ENC{$encname}
}

sub new {
    my ($class, %options) = @_;

    my $from          = $options{from};
    my $from_init     = $options{from_init};
    my $from_remember = $options{from_remember};

    my $to          = $options{to};
    my $to_init     = $options{to_init};
    my $to_remember = $options{to_remember};

    #
    # It is a non-sense to set from without to, or vice-versa
    #
    croak 'from and to options must be both set or unset' if ((defined($from) && ! defined($to)) && (defined($to) && ! defined($from)));
    
    return bless {
        from               => $from,
        from_remember      => $from_remember,
        from_enc           => defined($from) ? _get_enc($from) : undef,
        from_bookkeeping   => $from_remember ? $from_init : undef,
        to                 => $to,
        to_remember        => $to_remember,
        to_enc             => defined($to) ? _get_enc($to) : undef,
        to_bookkeeping     => $to_remember ? $to_init : undef,
        decodeBuffer       => $from_init,
        from_init          => $from_init,
        encodeBuffer       => $to_init,
        to_init            => $to_init,
        isCharacterStream  => defined($from) && defined($to)
    }, $class
}

=head3 from($self)

Returns the "from" encoding name, undef if none.

=cut

sub from {
    my ($self) = @_;

    return $self->{from}
}

=head3 to($self)

Returns the "to" encoding name, undef if none.

=cut

sub to {
    my ($self) = @_;

    return $self->{to}
}

=head3 from_init($self)

Returns a defined value if the initial input buffer is not yet consumed. This can happen only if the C<from_to> method() was never called and the constructor had a defined "from_init" option value.

=cut

sub have_initial_data {
    my ($self) = @_;

    return defined($self->{from_init}) || defined($self->{to_init})
}

=head3 isCharacterStream($self)

Returns a true value if there is charset convertion.

=cut

sub isCharacterStream {
    my ($self) = @_;

    return $self->{isCharacterStream}
}

=head3 from_bookkeeping($self)

Returns eventual octets bookkeeping in the C<from> charset.

=cut

sub from_bookkeeping {
    my ($self) = @_;

    return $self->{from_bookkeeping}
}

=head3 to_bookkeeping($self)

Returns eventual octets bookkeeping in the C<to> charset.

=cut

sub to_bookkeeping {
    my ($self) = @_;

    return $self->{to_bookkeeping}
}

=head3 from_init($self, $octets)

Returns the "from_init" constructor argument, eventually setting it if $octets is given in parameters. Dangerous method.

=cut

sub from_init {
    my $self = shift;

    if (@_) {
        my $octets = shift;
        # $log->tracef("Resetting 'from_init' with %d octets\n%s", bytes::length($octets));
        return $self->{from_init} = $octets
    } else {
        return $self->{from_init}
    }
}

=head3 to_init($self, $octets)

Returns the "to_init" constructor argument, eventually setting it if $octets is given in parameters. Dangerous method.

=cut

sub to_init {
    my $self = shift;

    if (@_) {
        my $octets = shift;
        # $log->tracef("Resetting 'to_init' with %d octets\n%s", bytes::length($octets));
        return $self->{to_init} = $octets
    } else {
        return $self->{to_init}
    }
}

=head3 to_bookkeeping($self)

Returns eventual octets bookkeeping in the C<to> charset.

=cut

sub from_to {
    my ($self, $from_octets) = @_;

    my $to_octets;

    $self->{from_bookkeeping} .= $from_octets if $self->{from_remember};   

    if ($self->{isCharacterStream}) {
        $self->{decodeBuffer} .= $from_octets;
        #if ($log->is_info) {
        #    $log->infof("Input octets in %s charset:\n%s", $self->{from}, HexDump($self->{decodeBuffer}));
        #}
        #$log->info('decode');
        $self->{encodeBuffer} .= $self->{from_enc}->decode($self->{decodeBuffer}, Encode::FB_WARN);
        #if ($log->is_trace) {
        #    $log->tracef("Input octets remaining after %s decode:\n%s", $self->{from}, HexDump($self->{decodeBuffer}));
        #    $log->tracef("Intermediate octets in perl's internal format after %s encode:\n%s", $self->{from}, HexDump($self->{encodeBuffer}));
        #}

        #$log->info('encode');
        $to_octets = $self->{to_enc}->encode($self->{encodeBuffer}, Encode::FB_WARN);
        #if ($log->is_trace) {
        #    $log->tracef("Intermediate octets remaining after %s encode:\n%s", $self->{to}, HexDump($self->{encodeBuffer}));
        #    $log->tracef("Output octets in %s charset:\n%s", $self->{to}, HexDump($to_octets));
        #}
    } else {
        $self->{decodeBuffer} .= $from_octets;
        $to_octets = $self->{decodeBuffer};
        $self->{decodeBuffer} = undef;
        #if ($log->is_trace) {
        #    $log->tracef("Output octets:\n%s", HexDump($to_octets));
        #}
    }

    $self->{to_bookkeeping} .= $to_octets if $self->{to_remember};

    #
    # Always reset from_init and to_init : they are consumed
    #
    $self->{from_init} = undef;
    $self->{to_init} = undef;

    # $log->infof("Output octets:\n%s", HexDump($to_octets));

    return $to_octets
}

=head1 SEE ALSO

L<Encode::Encoding>

=cut

1;
