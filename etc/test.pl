#!env perl
package MyReader::File;
use strict;
use diagnostics;
use Carp qw/croak/;
use Log::Any qw/$log/;

my $BUFLEN = 1024;

sub new {
    my ($class, $filename) = @_;
    croak "Missing filename" unless defined($filename);

    $log->tracef('Opening %s', $filename);
    open(my $fh, '< :raw :bytes', $filename) || croak "Cannot open $filename, $!";

    return bless { filename => $filename, fh => $fh, data => undef, isEof => undef }, $class
}

sub DESTROY {
    my ($self) = @_;

    $log->tracef('Closing %s', $self->{filename});
    close($self->{fh}) || warn sprintf('Failed to %s, %s', $self->{filename}, $!)
}

sub read {
    my ($self) = @_;

    return read($self->{fh}, $self->{data}, $BUFLEN);
}

sub data {
    my ($self) = @_;

    return $self->{data}
}

sub isEof {
    my ($self) = @_;

    return eof($self->{fh})
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

#
# Take care, this can contain UTF-8 stuff
#
use open ':std', ':encoding(UTF-8)';

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

