use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Base::Recognizer::Interface;
use Carp qw/croak/;
use Class::Tiny qw//, {
    reader                 => undef,
    initial_data           => undef,
    initial_eof            => undef,
    bookkeeping            => sub { my $self = shift; return $self->{_bookkeeping} },
    remember               => 0,
    isCharacterStream      => 1,
    encoding               => undef,
    isWithDisableThreshold => 0,
    isWithExhaustion       => 0,
    isWithNewline          => 0,
    isWithTrack            => 0
};
use Safe::Isa;

sub BUILD {
    my ($self) = @_;

    foreach (qw/read eof data/) {
        croak "reader must be able to do '_'" unless $self->reader->$_can($_)
    }
}

sub read {
    my ($self) = @_;

    return defined($self->initial_data) ? 1 : $self->reader->read
}

# ABSTRACT: XML recognizer interface base class

# VERSION

# AUTHORITY

=head1 DESCRIPTION

XML recognizer interface base class

  my $recognizerInterface = MarpaX::ESLIF::XML::Base::Recognizer::Interface->new(
                              reader                 => undef,
                              initial_data           => undef,
                              initial_eof            => undef,
                              bookkeeping            => undef,
                              remember               => 0,
                              isCharacterStream      => 1,
                              encoding               => undef,
                              isWithDisableThreshold => 0,
                              isWithExhaustion       => 0,
                              isWithNewline          => 0,
                              isWithTrack            => 0
                            );

=cut

=head1 ATTRIBUTES

=head2 initial_data

Initial data with which the recognizer will start. Used once and only if set to a defined value. Defaults to C<undef>.

=head2 initial_eof

Initial EOF state. Used once and only if set to a defined value. Defaults to C<undef>.

=head2 remember

A flag saying if this recognizer must remember all data that was read. Defaults to a false value.

=head2 bookkeeping

Returns all remembered data. Defaults to C<undef>.

=cut

sub data {
    my ($self) = @_;

    my $rc;
    if (defined(my $initial_data = $self->initial_data)) {
        $rc = $initial_data;
        $self->initial_data(undef)
    } else {
        $rc = $self->reader->data
    }
    #
    # Default value of "remember" is undef
    #
    $self->{_bookkeeping} .= $rc if $self->remember();
    return $rc
}

sub isEof {
    my ($self) = @_;

    my $rc;
    if (defined(my $initial_eof = $self->initial_eof)) {
        $rc = $initial_eof;
        $self->initial_eof(undef);
    } else {
        $rc = $self->reader->eof
    }

    return $rc
};

1;
