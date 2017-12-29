#!env perl
package MyReader::File;
use strict;
use diagnostics;
use Carp qw/croak/;
use IO::File;
use Log::Any qw/$log/;

my $BUFLEN = 1024;

sub new {
    my ($class, $filename) = @_;
    croak "Missing filename" unless defined($filename);

    my $fh = IO::File->new($filename, 'r') || croak "Cannot open $filename, $!";
    $fh->binmode || croak "binmode failure on $filename, $!";

    return bless { fh => $fh, data => undef, isEof => undef }, $class
}

sub DESTROY {
    my ($self) = @_;

    $self->{fh}->close
}

sub read {
    my ($self) = @_;

    $log->tracef("Reading %d bytes", $BUFLEN);
    return $self->{fh}->read($self->{data}, $BUFLEN)
}

sub byte {
    my ($self) = @_;

    $log->trace("Reading a byte");
    return $self->{fh}->read($self->{data}, 1)
}

sub data {
    my ($self) = @_;

    $log->trace("Return data");
    return $self->{data}
}

sub isEof {
    my ($self) = @_;

    $log->tracef("Returning eof = %s", $self->{fh}->eof);
    return $self->{fh}->eof
}

package main;
use strict;
use diagnostics;
use Log::Log4perl qw/:easy/;
use Log::Any::Adapter;
use Log::Any qw/$log/;
use MarpaX::ESLIF::XML::XML10;
use Try::Tiny;

#
# Init log
#
our $defaultLog4perlConf = '
log4perl.rootLogger              = TRACE, Screen
log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr  = 1
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
';
Log::Log4perl::init(\$defaultLog4perlConf);
Log::Any::Adapter->set('Log4perl');

foreach (@ARGV) {
    my $filename = shift;
    next unless $filename =~ /\.xml$/i;
    $log->infof("Parsing %s", $filename);
    try {
        my $reader = MyReader::File->new($filename);
        MarpaX::ESLIF::XML::XML10->new(reader => $reader)->parse;
    } catch {
        $log->errorf('%s', $_);
    };
}

