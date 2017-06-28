#!env perl
use strict;
use diagnostics;
use Log::Any qw/$log/;
use Log::Any::Adapter ('Stderr', log_level => 'trace' );
use MarpaX::ESLIF::XML;

my $file = shift || die "Usage: $0 filepath.xml";

open(my $fh, '<', $file) || die "Cannot open $file, $!";
my $data = do { local $/; <$fh> };
close($fh) || warn "Cannot close $file, $!";

MarpaX::ESLIF::XML::XML10->new(logger => $log)->parse($data);
