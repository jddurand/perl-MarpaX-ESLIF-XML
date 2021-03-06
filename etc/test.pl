#!env perl
package MyReader::File;
use strict;
use diagnostics;
use Carp qw/croak/;
use Log::Any qw/$log/;

my $BUFLEN = 1024 * 1024;

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
    close($self->{fh}) || warn sprintf('Failed to close %s, %s', $self->{filename}, $!)
}

sub read {
    my ($self) = @_;

    return read($self->{fh}, $self->{data}, $BUFLEN);
}

sub eof {
    my ($self) = @_;

    return eof($self->{fh})
}

sub data {
    my ($self) = @_;

    return $self->{data}
}

package main;
use strict;
use diagnostics;
use Log::Log4perl qw/:easy/;
use Log::Any::Adapter;
use Log::Any qw/$log/;

BEGIN {
    #
    # Init log
    #
    our $defaultLog4perlConf = '
log4perl.rootLogger                               = INFO, Screen
log4perl.appender.Screen                          = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr                   = 1
log4perl.appender.Screen.layout                   = PatternLayout
log4perl.appender.Screen.Threshold                = INFO
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
        ';
    Log::Log4perl::init(\$defaultLog4perlConf);
    Log::Any::Adapter->set('Log4perl');
}

use MarpaX::ESLIF::XML::XML10;
use Try::Tiny;
use IPC::System::Simple qw/capturex/;

#
# Take care, this can contain UTF-8 stuff
#
use open ':std', ':encoding(UTF-8)';

foreach (@ARGV) {
    my $filename = shift;
    next unless -r $filename;
    # next if -s $filename > 1024*1024;
    next unless $filename =~ /\.xml$/i;
    next unless -s $filename;
    $log->infof("Parsing %s", $filename);
    #try {
        # $Log::Log4perl::Logger::APPENDER_BY_NAME{'Screen'}->threshold('TRACE');
    my $eslifrc = MarpaX::ESLIF::XML::XML10->new(reader => MyReader::File->new($filename))->parse;
    undef $@;
    eval { capturex("xmllint", $filename) };
    my $xmllintrc = $@ ? 0 : 1;
        if ($eslifrc && ! $xmllintrc) {
            print STDERR "************* $filename: ESLIF success but XMLLINT failure\n$@\n";
        }
        elsif (! $eslifrc && $xmllintrc) {
            print STDERR "************* $filename: XMLLINT success but ESLIF failure\n";
        }
    last;
    #} catch {
    #    $log->errorf('%s', $_);
    #};
}

