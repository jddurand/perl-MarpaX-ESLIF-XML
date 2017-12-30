use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::XML10;
use Carp qw/croak/;
use Data::Section -setup;
use I18N::Charset qw/iana_charset_name/;
use Log::Any '$log', filter => \&_log_filter;
use MarpaX::ESLIF;
use MarpaX::ESLIF::XML::RecognizerInterface;
use MarpaX::ESLIF::XML::RecognizerInterface2;
use MarpaX::ESLIF::XML::ValueInterface::BOM;
use MarpaX::ESLIF::XML::ValueInterface::Decl;
use MarpaX::ESLIF::XML::ValueInterface::Guess;
use MarpaX::ESLIF::XML::ValueInterface;
use MarpaX::ESLIF::XML::Encode;
use Safe::Isa;

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

# ABSTRACT: XML 1.0 parser

# VERSION

# AUTHORITY

=head1 DESCRIPTION

XML 1.0 parser.

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML10;

    my $eslifxml10 = MarpaX::ESLIF::XML::XML10->new();
    my $input      = '<?xml></xml>';
    my $xmlhash    = $eslifxml10->parse($input);

=cut

my $ESLIF         = MarpaX::ESLIF->new($log);

my $BOM_SOURCE    = ${__PACKAGE__->section_data('BOM')};
my $BOM_GRAMMAR   = MarpaX::ESLIF::Grammar->new($ESLIF, $BOM_SOURCE);

my $GUESS_SOURCE  = ${__PACKAGE__->section_data('Guess')};
my $GUESS_GRAMMAR = MarpaX::ESLIF::Grammar->new($ESLIF, $GUESS_SOURCE);

my $XML10_SOURCE  = ${__PACKAGE__->section_data('XML 1.0')};
my $XML10_GRAMMAR = MarpaX::ESLIF::Grammar->new($ESLIF, $XML10_SOURCE);

my $DECL_SOURCE   = ":start ::= XMLDecl\n$XML10_SOURCE";
$DECL_SOURCE      =~ s/## Decl_//g; # Enable specific decl actions
my $DECL_GRAMMAR  = MarpaX::ESLIF::Grammar->new($ESLIF, $DECL_SOURCE);

#
# XML grammar is highly recursive on "element": since we are doing SAX stuff, no
# need to impose that to the grammar. Take care we want to start the element
# EXACTLY when there is a start tag. This is why '<' is expressed in the form
# of the STAG lexeme. The end with at ETag completion.
#
my $ELEMENT_SOURCE   = ":start ::= element\n$XML10_SOURCE";
my $ELEMENT_GRAMMAR  = MarpaX::ESLIF::Grammar->new($ESLIF, $ELEMENT_SOURCE);

sub new {
    my ($class, %options) = @_;

    return bless {
        reader   => $options{reader},
        encoding => $options{encoding}
    }, $class
}

sub _log_filter {
    my ($category, $level, $msg) = @_;

    return if $MarpaX::Logger::XML::Logger::Silent;
    return $msg;
}

sub _charset_from_bom {
    my ($self, $encode) = @_;

    local $MarpaX::Logger::XML::Logger::Silent = 1;
    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface2->new(encode => $encode, reader => $self->{reader}, isWithExhaustion => 1);
    my $valueInterface = MarpaX::ESLIF::XML::ValueInterface::BOM->new();

    if ($BOM_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        my ($encoding, $bytes) = @{$valueInterface->getResult};
        #
        # Get data that has to be reinjected
        #
        my $bookkeeping = $encode->from_bookkeeping();
        #
        # ... Minus the number of bytes used by the BOM
        #
        substr($bookkeeping, 0, $bytes, '');
        my $charset = $self->_iana_charset($encoding);

        return ($charset, $bookkeeping)
    }

    return (undef, $encode->from_bookkeeping())
}

sub _iana_charset {
    my ($self, $encoding) = @_;

    return undef unless defined($encoding);
    #
    # Common pitfalls
    #
    if (lc($encoding) eq 'unicode') { # $encoding contains only ASCII characters
        $log->warnf('Invalid encoding specification "%s", interpreted as "UTF-16"', $encoding);
        return 'UTF-16'
    }
    #
    # This should never fail
    #
    my $charset = iana_charset_name($encoding) || croak "Failed to get charset name from $encoding";

    return $charset
}

sub _charset_from_guess {
    my ($self, $encode) = @_;

    local $MarpaX::Logger::XML::Logger::Silent = 1;
    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface2->new(encode => $encode, reader => $self->{reader}, isWithExhaustion => 1);
    my $valueInterface = MarpaX::ESLIF::XML::ValueInterface::Guess->new();

    if ($GUESS_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        my ($encoding, $bytes) = @{$valueInterface->getResult};
        #
        # Get data that has to be reinjected
        #
        my $bookkeeping = $encode->from_bookkeeping();
        my $charset = $self->_iana_charset($encoding);

        return ($charset, $bookkeeping)
    }

    return (undef, $encode->from_bookkeeping())
}

sub _charset_from_decl {
    my ($self, $encode) = @_;

    local $MarpaX::Logger::XML::Logger::Silent = 1;
    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface2->new(encode => $encode, reader => $self->{reader}, isWithExhaustion => 1, isCharacterStream => 1);
    my $valueInterface = MarpaX::ESLIF::XML::ValueInterface::Decl->new();

    if ($DECL_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        my $encoding = $valueInterface->getResult;
        #
        # Get data that has to be reinjected
        #
        my $bookkeeping = $encode->from_bookkeeping();
        my $charset = $self->_iana_charset($encoding);

        return ($charset, $bookkeeping)
    }

    return (undef, $encode->from_bookkeeping())
}

sub _encoding {
    my ($self, $charset_from_bom, $charset_from_guess, $charset_from_decl) = @_;

    $log->debugf("Merging encodings from BOM: %s, Guess: %s and Declaration: %s", $charset_from_bom, $charset_from_guess, $charset_from_decl);
    my $encoding;
    if (! defined($charset_from_bom)) {
        if (! defined($charset_from_guess) || ! defined($charset_from_decl)) {
            $encoding = 'UTF-8';
        } else {
            if (($charset_from_decl eq 'UTF-16') && ($charset_from_guess eq 'UTF-16BE' || $charset_from_guess eq 'UTF-16LE')) {
                $encoding =  $charset_from_guess;
            } elsif (($charset_from_decl eq 'UTF-32') && ($charset_from_guess eq 'UTF-32BE' || $charset_from_guess eq 'UTF-32LE')) {
                $encoding =  $charset_from_guess;
            } else {
                $encoding = $charset_from_decl;
            }
        }
    } else {
        if ($charset_from_bom eq 'UTF-8') {
            if (defined($charset_from_guess) && $charset_from_guess ne 'UTF-8') {
                croak "Encoding mismatch between BOM $charset_from_bom and guess $charset_from_guess";
            }
            if (defined($charset_from_decl) && $charset_from_decl ne 'UTF-8') {
                croak "Encoding mismatch between BOM $charset_from_bom and declaration $charset_from_decl";
            }
            $encoding = 'UTF-8';
        } else {
            if ($charset_from_bom eq 'UTF-16BE' or $charset_from_bom eq 'UTF-16LE') {
                if (defined($charset_from_guess) && $charset_from_guess ne $charset_from_bom) {
                    croak "Encoding mismatch between BOM $charset_from_bom and guess $charset_from_guess";
                }
                if (defined($charset_from_decl) && ($charset_from_decl ne 'UTF-16' and $charset_from_decl ne $charset_from_bom)) {
                    croak "Encoding mismatch between BOM $charset_from_bom and declaration $charset_from_decl";
                }
                $encoding = $charset_from_bom;
            } elsif ($charset_from_bom eq 'UTF-32BE' or $charset_from_bom eq 'UTF-32LE') {
                if (defined($charset_from_guess) && $charset_from_guess ne $charset_from_bom) {
                    croak "Encoding mismatch between BOM $charset_from_bom and guess $charset_from_guess";
                }
                if (defined($charset_from_decl) && ($charset_from_decl ne 'UTF-32' and $charset_from_decl ne $charset_from_bom)) {
                    croak "Encoding mismatch between BOM $charset_from_bom and declaration $charset_from_decl";
                }
                $encoding = $charset_from_bom;
            } else {
                croak 'Encoding setup failed';
            }
        }
    }

    return $encoding
}

sub parse {
    my ($self) = @_;
    #
    # Get encoding from BOM, guess and declaration. We remember octets already read at every step
    #
    my ($charset_from_bom, $charset_from_guess, $charset_from_decl, $from_init);
    #
    # BOM
    #
    my $encode = MarpaX::ESLIF::XML::Encode->new(from_remember => 1);
    ($charset_from_bom, $from_init) = $self->_charset_from_bom($encode);
    $log->debugf("Encoding from BOM: %s, bookkeeping: %d bytes", $charset_from_bom, bytes::length($from_init));
    #
    # Guess
    #
    $encode = MarpaX::ESLIF::XML::Encode->new(from_remember => 1, from_init => $from_init);
    ($charset_from_guess, $from_init) = $self->_charset_from_guess($encode);
    $log->debugf("Encoding from Guess: %s, bookkeeping: %d bytes", $charset_from_guess, bytes::length($from_init));
    #
    # Declaration
    #
    $encode = MarpaX::ESLIF::XML::Encode->new(from_remember => 1, from_init => $from_init, from => $charset_from_bom // $charset_from_guess);
    ($charset_from_decl, $from_init)  = $self->_charset_from_decl($encode);
    $log->debugf("Encoding from Declaration: %s, bookkeeping: %d bytes", $charset_from_decl, bytes::length($from_init));
    #
    # Algorithm "Raw XML charset encoding detection"
    # https://rometools.github.io/rome/RssAndAtOMUtilitiEsROMEV0.5AndAboveTutorialsAndArticles/XMLCharsetEncodingDetectionHowRssAndAtOMUtilitiEsROMEHelpsGettingTheRightCharsetEncoding.html
    #
    my $charset = $self->_encoding($charset_from_bom, $charset_from_guess, $charset_from_decl);
    $log->debugf("Charset used: %s", $charset);
    #
    # XML itself
    #
    $encode = MarpaX::ESLIF::XML::Encode->new(from_init => $from_init, from => $charset, to => 'UTF-8');
    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface2->new(encode => $encode, reader => $self->{reader}, newline => 1);
    my $valueInterface = MarpaX::ESLIF::XML::ValueInterface->new();
    my $eslifRecognizer = MarpaX::ESLIF::Recognizer->new($XML10_GRAMMAR, $recognizerInterface);
    #
    # Because of SAX events, we use explicitly the recognizer.
    #
    if ($eslifRecognizer->scan()) {
        $self->_manage_events($eslifRecognizer, $recognizerInterface, $encode);
        while ($eslifRecognizer->isCanContinue) {
            last unless $eslifRecognizer->resume;
            $self->_manage_events($eslifRecognizer, $recognizerInterface, $encode);
        }
    }

    return MarpaX::ESLIF::Value->new($eslifRecognizer, $valueInterface)->value();
}

sub _manage_events {
    my ($self, $eslifRecognizer, $recognizerInterface, $encode) = @_;
    #
    # Remember, events are always sorted like this:
    # - completion, then
    # - nullable, then
    # - prediction
    foreach (@{$eslifRecognizer->events()}) {
        my $event = $_->{event};
        my $symbol = $_->{symbol};

        my $input = $eslifRecognizer->input // '';
        $log->debugf('[%s] Event %s on symbol %s', substr($input, 0, 16), $event, $symbol);
        if ($event eq 'PITarget$') {
            my $value = $eslifRecognizer->lexemeLastPause($symbol);
            $log->debugf('Event %s on symbol %s, value: %s', $event, $symbol, $value);
            if ($value =~ /^xml$/i) {
                my @location = $eslifRecognizer->location();
                if (@location) {
                    croak "PITarget cannot be '$value' at line $location[0], column $location[1]";
                } else {
                    croak "PITarget cannot be '$value'";
                }
            }
        } elsif ($event eq '^STAG') {
            $log->debugf('Parsing element separately');
            #
            # We get in return the input buffer just after element valuation
            #
            my ($value, $utf8bytes_consumed) = $self->parse_element($eslifRecognizer, $encode);
            $log->debugf('Last element parsing has consumed %d bytes', $utf8bytes_consumed);
            #
            # We say to recognizer to jump over them
            #
            $eslifRecognizer->lexemeRead('ELEMENT', $value, $utf8bytes_consumed);
        } elsif ($event eq 'ETag$') {
            $log->debug('Current Element end');
            return;
        }
    }
}

sub parse_element {
    my ($self, $eslifRecognizerParent, $encode) = @_;
    #
    # Current settings
    #
    $encode = MarpaX::ESLIF::XML::Encode->new(from => $encode->from, to_remember => 1, to => 'UTF-8', to_init => $eslifRecognizerParent->input());
    #
    # element itself
    #
    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface2->new(encode => $encode, reader => $self->{reader}, isWithExhaustion => 1, isWithTrack => 1, newline => 1);
    my $valueInterface = MarpaX::ESLIF::XML::ValueInterface->new();
    my $eslifRecognizer = MarpaX::ESLIF::Recognizer->new($ELEMENT_GRAMMAR, $recognizerInterface);
    #
    # element starts with STAG on which there is a predicted event. It is guaranteed to there. In UTF-8 this is a single byte.
    #
    $eslifRecognizer->lexemeRead('STAG', '<', 1);
    #
    # Because of SAX events, we use explicitly the recognizer.
    #
    if ($eslifRecognizer->scan()) {
        $self->_manage_events($eslifRecognizer, $recognizerInterface, $encode);
        while ($eslifRecognizer->isCanContinue) {
            last unless $eslifRecognizer->resume;
            $self->_manage_events($eslifRecognizer, $recognizerInterface, $encode);
        }
    }

    my @location = $eslifRecognizer->lastCompletedLocation('element');
    $log->debugf("Last element location: %s", \@location);
    die "Start location of last element should be 0..." if $location[0];

    my $to_bookkeeping = $encode->to_bookkeeping();
    my $inputInUtf8 = substr($to_bookkeeping, $location[0], $location[1]);
    $log->debugf("Calling for value on:\n%s", $inputInUtf8);

    my $value = MarpaX::ESLIF::Value->new($eslifRecognizer, $valueInterface)->value() // croak "Parse failure";
    #
    # We return the value and the number of UTF-8 bytes consumed
    #
    return ($value, $location[1])
}

1;

__DATA__
__[ BOM ]__
#
# Unusual ordering is not considered
#
BOM ::= [\x{00}] [\x{00}] [\x{FE}] [\x{FF}] action => UTF_32BE
      | [\x{FF}] [\x{FE}] [\x{00}] [\x{00}] action => UTF_32LE
      | [\x{FE}] [\x{FF}]                   action => UTF_16BE
      | [\x{FF}] [\x{FE}]                   action => UTF_16LE
      | [\x{EF}] [\x{BB}] [\x{BF}]          action => UTF_8

__[ Guess ]__
#
# Unusual ordering is not considered nor EBCDIC with code page
#
Guess ::= [\x{00}] [\x{00}] [\x{00}] [\x{3C}] action => UTF_32BE # '<'
        | [\x{3C}] [\x{00}] [\x{00}] [\x{00}] action => UTF_32LE # '<'
        | [\x{00}] [\x{3C}] [\x{00}] [\x{3F}] action => UTF_16BE # '<?'
        | [\x{3C}] [\x{00}] [\x{3F}] [\x{00}] action => UTF_16LE # '<?'
        | [\x{3C}] [\x{3F}] [\x{78}] [\x{6D}] action => UTF_8    # '<?x'

__[ XML 1.0 ]__
#
# From https://www.w3.org/TR/REC-xml (5th edition)
#
# Take care, original has several ambiguities:
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0000 (applied)
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001 (applied)
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002 (applied)
#
# Note:
# Concerning XML Exceptions there are three categories:
# - Comment interior: We create a character class without for Char minus the '-' character
# - PITarget        : Native BNF exception is doing it
# - Others          : They are ALL in the form: <character> - ( <character>* <exception longer than one character> <character>* )
#                     where <exception longer than one character> is always an expected terminal preceeding and/or succeeding <character>* !
#                     So this mean there is NO needed to write exception...: the grammar will natively stop <character>* parsing as soon
#                     as it sees <exception longer than one character> in stream, because it is always working in the LATM (Longest Acceptable
#                     Token Match) mode...

event document$ = completed document
document           ::= prolog element <Misc any>
event Char$ = completed Char
Char               ::= [\x{9}\x{A}\x{D}\x{20}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u
event S1$ = completed S1
S1                 ::= [\x{20}\x{9}\x{D}\x{A}]:u
event S$ = completed S
S                  ::= S1+
event NameStartChar$ = completed NameStartChar
NameStartChar      ::= [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
event NameChar$ = completed NameChar
NameChar           ::= [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
event Name$ = completed Name
Name               ::= NameStartChar <NameChar any>
event Names$ = completed Names
Names              ::= Name+ separator => [\x{20}]:u
event Nmtoken$ = completed Nmtoken
Nmtoken            ::= NameChar+
event Nmtokens$ = completed Nmtokens
Nmtokens           ::= Nmtoken+ separator => [\x{20}]:u
event EntityValue$ = completed EntityValue
EntityValue        ::= '"' <EntityValue1 any>   '"' | "'" <EntityValue2 any>   "'"
event AttValue$ = completed AttValue
AttValue           ::= '"' <AttValue1 any>      '"' | "'" <AttValue2 any>      "'"
event SystemLiteral$ = completed SystemLiteral
SystemLiteral      ::= '"' <SystemLiteral1 any> '"' | "'" <SystemLiteral2 any> "'"
event PubidLiteral$ = completed PubidLiteral
PubidLiteral       ::= '"' <PubidChar1 any>     '"' | "'" <PubidChar2 any>     "'"
event PubidChar$ = completed PubidChar
PubidChar          ::= [\x{20}\x{D}\x{A}a-zA-Z0-9\-'()+,./:=?;!*#@$_%]
event CharData$ = completed CharData
CharData           ::= <CharData Exceptioned>
event Comment$ = completed Comment
Comment            ::= '<!--' <Comment Interior> '-->'
event PI$ = completed PI
PI                 ::= '<?' PITarget                    '?>'
                     | '<?' PITarget S <PI Exceptioned> '?>'
event CDSect$ = completed CDSect
CDSect             ::= CDStart CData CDEnd
event CDStart$ = completed CDStart
CDStart            ::= '<![CDATA['
event CData$ = completed CData
CData              ::= <CData Exceptioned>
event CDEnd$ = completed CDEnd
CDEnd              ::= ']]>'
event prolog$ = completed prolog
prolog             ::= <XMLDecl maybe> <Misc any>
                     | <XMLDecl maybe> <Misc any> doctypedecl <Misc any>
event XMLDecl$ = completed XMLDecl
XMLDecl            ::= '<?xml' VersionInfo <EncodingDecl maybe> <SDDecl maybe> <S maybe> '?>' ## Decl_action => ::copy[2]
event VersionInfo$ = completed VersionInfo
VersionInfo        ::= S 'version' Eq "'" VersionNum "'"
                     | S 'version' Eq '"' VersionNum '"'
event Eq$ = completed Eq
Eq                 ::= <S maybe> '=' <S maybe>
event VersionNum$ = completed VersionNum
VersionNum         ::= '1.' <digit many>
#
# https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002
#
# Use S1 instead of S in Misc
#
event Misc$ = completed Misc
Misc               ::= Comment
                     | PI
                     | S1
event doctypedecl$ = completed doctypedecl
doctypedecl        ::= '<!DOCTYPE' S Name              <S maybe>                             '>'
                     | '<!DOCTYPE' S Name              <S maybe> '[' intSubset ']' <S maybe> '>'
                     | '<!DOCTYPE' S Name S ExternalID <S maybe>                             '>'
                     | '<!DOCTYPE' S Name S ExternalID <S maybe> '[' intSubset ']' <S maybe> '>'
#
# https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001
#
# Change S in DeclSep to S1
#
event DeclSep$ = completed DeclSep
DeclSep            ::= PEReference
                     | S1
event intSubset$ = completed intSubset
intSubset          ::= <intSubset1 any>
event markupdecl$ = completed markupdecl
markupdecl         ::= elementdecl
                     | AttlistDecl
                     | EntityDecl
                     | NotationDecl
                     | PI
                     | Comment
event extSubset$ = completed extSubset
extSubset          ::=          extSubsetDecl
                     | TextDecl extSubsetDecl
event extSubsetDecl$ = completed extSubsetDecl
extSubsetDecl      ::= <extSubsetDecl1 any>
event SDDecl$ = completed SDDecl
SDDecl             ::= S 'standalone' Eq "'" <yes or no> "'"
                     | S 'standalone' Eq '"' <yes or no> '"'
event element$ = completed element
element            ::= EmptyElemTag
                     | STag content ETag
                     | ELEMENT                                    # A lexeme that never matches, used to skip consumed bytes
event STag$ = completed STag
STag               ::= STAG Name <STag1 any> <S maybe> '>'
event Attribute$ = completed Attribute
Attribute          ::= Name Eq AttValue
event ETag$ = completed ETag
ETag               ::= '</' Name <S maybe> '>'
event content$ = completed content
content            ::= <CharData maybe> <content1 any>
event EmptyElemTag$ = completed EmptyElemTag
EmptyElemTag       ::= STAG Name <EmptyElemTag1 any> <S maybe> '/>'
event elementdecl$ = completed elementdecl
elementdecl        ::= '<!ELEMENT' S Name S contentspec <S maybe> '>'
event contentspec$ = completed contentspec
contentspec        ::= 'EMPTY' | 'ANY' | Mixed | children 
event children$ = completed children
children           ::= <choice or seq> <sequence maybe>
event cp$ = completed cp
cp                 ::= <Name or choice or seq> <sequence maybe>
event choice$ = completed choice
choice             ::= '(' <S maybe> cp <choice1 many> <S maybe> ')'
event seq$ = completed seq
seq                ::= '(' <S maybe> cp <seq1 any>     <S maybe> ')'
event Mixed$ = completed Mixed
Mixed              ::= '(' <S maybe> '#PCDATA' <Mixed1 any> <S maybe> ')*'
                     | '(' <S maybe> '#PCDATA'              <S maybe> ')'
event AttlistDecl$ = completed AttlistDecl
AttlistDecl        ::= '<!ATTLIST' S Name <AttDef any> <S maybe> '>'
event AttDef$ = completed AttDef
AttDef             ::= S Name S AttType S DefaultDecl 
event AttType$ = completed AttType
AttType            ::= StringType | TokenizedType | EnumeratedType
event StringType$ = completed StringType
StringType         ::= 'CDATA'
event TokenizedType$ = completed TokenizedType
TokenizedType      ::= 'ID'
                     | 'IDREF'
                     | 'IDREFS'
                     | 'ENTITY'
                     | 'ENTITIES'
                     | 'NMTOKEN'
                     | 'NMTOKENS'
event EnumeratedType$ = completed EnumeratedType
EnumeratedType     ::= NotationType
                     | Enumeration
event NotationType$ = completed NotationType
NotationType       ::= 'NOTATION' S '(' <S maybe> Name    <NotationType1 any> <S maybe> ')'
event Enumeration$ = completed Enumeration
Enumeration        ::=              '(' <S maybe> Nmtoken <Enumeration1 any>  <S maybe> ')'
event DefaultDecl$ = completed DefaultDecl
DefaultDecl        ::= '#REQUIRED'
                     | '#IMPLIED'
                     |            AttValue
                     | '#FIXED' S AttValue
event conditionalSect$ = completed conditionalSect
conditionalSect    ::= includeSect | ignoreSect
event includeSect$ = completed includeSect
includeSect        ::= '<![' <S maybe> 'INCLUDE' <S maybe> '[' extSubsetDecl ']]>'
#
# The rule <ignoreSectContents any>  ::= ignoreSectContents* will trigger MARPA_ERR_COUNTED_NULLABLE: Nullable symbol on RHS of a sequence rule
# because ignoreSectContents is a nullable, so we revisit the whole ignore sections by making
# Ignore not nullable.
#
# ORIGINAL:
# ignoreSect         ::= '<![' <S maybe> 'IGNORE' <S maybe> '[' <ignoreSectContents any> ']]>'
# ignoreSectContents ::= Ignore <ignoreSectContents1 any>
# Ignore             ::= <CHARDATA any> - <IGNORE EXCEPTION>
# Ignore             ::= # Because a lexeme cannot be a nullable

event ignoreSect$ = completed ignoreSect
ignoreSect         ::= '<![' <S maybe> 'IGNORE' <S maybe> '[' <ignoreSectContents any> ']]>'
                     | '<![' <S maybe> 'IGNORE' <S maybe> '['                          ']]>'
event ignoreSectContents$ = completed ignoreSectContents
ignoreSectContents ::= Ignore <ignoreSectContents1 any>
event Ignore$ = completed Ignore
Ignore             ::= <Ignore Exceptioned>
event CharRef$ = completed CharRef
CharRef            ::= '&#' <digit many> ';'
                     | '&#x' <hexdigit many> ';'
event Reference$ = completed Reference
Reference          ::= EntityRef | CharRef
event EntityRef$ = completed EntityRef
EntityRef          ::= '&' Name ';'
event PEReference$ = completed PEReference
PEReference        ::= '%' Name ';'
event EntityDecl$ = completed EntityDecl
EntityDecl         ::= GEDecl | PEDecl
event GEDecl$ = completed GEDecl
GEDecl             ::= '<!ENTITY' S Name S EntityDef <S maybe> '>'
event PEDecl$ = completed PEDecl
PEDecl             ::= '<!ENTITY' S '%' S Name S PEDef <S maybe> '>'
event EntityDef$ = completed EntityDef
EntityDef          ::= EntityValue
                     | ExternalID
                     | ExternalID NDataDecl
event PEDef$ = completed PEDef
PEDef              ::= EntityValue | ExternalID 
event ExternalID$ = completed ExternalID
ExternalID         ::= 'SYSTEM' S SystemLiteral
                     | 'PUBLIC' S PubidLiteral S SystemLiteral
event NDataDecl$ = completed NDataDecl
NDataDecl          ::= S 'NDATA' S Name 
event TextDecl$ = completed TextDecl
TextDecl           ::= '<?xml' <VersionInfo maybe> EncodingDecl <S maybe> '?>'
event extParsedEnt$ = completed extParsedEnt
extParsedEnt       ::= <TextDecl maybe> content 
event EncodingDecl$ = completed EncodingDecl
EncodingDecl       ::= S 'encoding' Eq '"' EncName '"'                               ## Decl_action => ::copy[4]
                     | S 'encoding' Eq "'" EncName "'"                               ## Decl_action => ::copy[4]
event EncName$ = completed EncName
EncName            ::= <EncName header> <EncName trailer any>
event NotationDecl$ = completed NotationDecl
NotationDecl       ::= '<!NOTATION' S Name S ExternalID <S maybe> '>' 
                     | '<!NOTATION' S Name S PublicID   <S maybe> '>' 
event PublicID$ = completed PublicID
PublicID           ::= 'PUBLIC' S PubidLiteral 

event Misc_any$ = completed <Misc any>
<Misc any>                ::= Misc*
event NameChar_any$ = completed <NameChar any>
<NameChar any>            ::= NameChar*
event EntityValue1$ = completed <EntityValue1>
<EntityValue1>            ::= [^%&"] | PEReference | Reference
event EntityValue2$ = completed <EntityValue2>
<EntityValue2>            ::= [^%&'] | PEReference | Reference
event EntityValue1_any$ = completed <EntityValue1 any>
<EntityValue1 any>        ::= <EntityValue1>*
event EntityValue2_any$ = completed <EntityValue2 any>
<EntityValue2 any>        ::= <EntityValue2>*
event AttValue1$ = completed <AttValue1>
<AttValue1>               ::= [^<&"] | Reference
event AttValue2$ = completed <AttValue2>
<AttValue2>               ::= [^<&'] | Reference
event AttValue1_any$ = completed <AttValue1 any>
<AttValue1 any>           ::= <AttValue1>*
event AttValue2_any$ = completed <AttValue2 any>
<AttValue2 any>           ::= <AttValue2>*
event SystemLiteral1$ = completed <SystemLiteral1>
<SystemLiteral1>          ::= [^"]
event SystemLiteral2$ = completed <SystemLiteral2>
<SystemLiteral2>          ::= [^']
event SystemLiteral1_any$ = completed <SystemLiteral1 any>
<SystemLiteral1 any>      ::= <SystemLiteral1>*
event SystemLiteral2_any$ = completed <SystemLiteral2 any>
<SystemLiteral2 any>      ::= <SystemLiteral2>*
event PubidChar1_any$ = completed <PubidChar1 any>
<PubidChar1 any>          ::= PubidChar*
event PubidChar2$ = completed <PubidChar2>
<PubidChar2>              ::= [\x{20}\x{D}\x{A}a-zA-Z0-9\-()+,./:=?;!*#@$_%]  # Same as PubidChar but without '
event PubidChar2_any$ = completed <PubidChar2 any>
<PubidChar2 any>          ::= <PubidChar2>*
event XMLDecl_maybe$ = completed <XMLDecl maybe>
<XMLDecl maybe>           ::= XMLDecl
<XMLDecl maybe>           ::=
event EncodingDecl_maybe$ = completed <EncodingDecl maybe>
<EncodingDecl maybe>      ::= EncodingDecl ## Decl_action => ::shift
<EncodingDecl maybe>      ::=
event SDDecl_maybe$ = completed <SDDecl maybe>
<SDDecl maybe>            ::= SDDecl
<SDDecl maybe>            ::= 
event S_maybe$ = completed <S maybe>
<S maybe>                 ::= S
<S maybe>                 ::= 
event digit$ = completed <digit>
<digit>                   ::= [0-9]
event digit_many$ = completed <digit many>
<digit many>              ::= <digit>+
event hexdigit$ = completed <hexdigit>
<hexdigit>                ::= [0-9a-fA-F]
event hexdigit_many$ = completed <hexdigit many>
<hexdigit many>           ::= <hexdigit>+
event intSubset1$ = completed <intSubset1>
<intSubset1>              ::= markupdecl
                            | DeclSep
event intSubset1_any$ = completed <intSubset1 any>
<intSubset1 any>          ::= <intSubset1>*
event extSubsetDecl1$ = completed <extSubsetDecl1>
<extSubsetDecl1>          ::= markupdecl
                            | conditionalSect
                            | DeclSep
event extSubsetDecl1_any$ = completed <extSubsetDecl1 any>
<extSubsetDecl1 any>      ::= <extSubsetDecl1>*
event yes_or_no$ = completed <yes or no>
<yes or no>               ::= 'yes' | 'no'
event STag1$ = completed <STag1>
<STag1>                   ::= S Attribute
event STag1_any$ = completed <STag1 any>
<STag1 any>               ::= <STag1>*
event CharData_maybe$ = completed <CharData maybe>
<CharData maybe>          ::= CharData
<CharData maybe>          ::=
event content1$ = completed <content1>
<content1>                ::= element   <CharData maybe>
                            | Reference <CharData maybe>
                            | CDSect    <CharData maybe>
                            | PI        <CharData maybe>
                            | Comment   <CharData maybe>
event content1_any$ = completed <content1 any>
<content1 any>            ::= <content1>*
event EmptyElemTag1$ = completed <EmptyElemTag1>
<EmptyElemTag1>           ::= S Attribute
event EmptyElemTag1_any$ = completed <EmptyElemTag1 any>
<EmptyElemTag1 any>       ::= <EmptyElemTag1>*
event choice_or_seq$ = completed <choice or seq>
<choice or seq>           ::= choice | seq
event sequence$ = completed <sequence>
<sequence>                ::= '?' | '*' | '+'
event sequence_maybe$ = completed <sequence maybe>
<sequence maybe>          ::= <sequence>
<sequence maybe>          ::=
event Name_or_choice_or_seq$ = completed <Name or choice or seq>
<Name or choice or seq>   ::= Name | choice | seq
event choice1$ = completed <choice1>
<choice1>                 ::= <S maybe> '|' <S maybe> cp
event choice1_many$ = completed <choice1 many>
<choice1 many>            ::= <choice1>+
event seq1$ = completed <seq1>
<seq1>                    ::= <S maybe> ',' <S maybe> cp
event seq1_any$ = completed <seq1 any>
<seq1 any>                ::= <seq1>*
event Mixed1$ = completed <Mixed1>
<Mixed1>                  ::= <S maybe> '|' <S maybe> Name
event Mixed1_any$ = completed <Mixed1 any>
<Mixed1 any>              ::= <Mixed1>*
event AttDef_any$ = completed <AttDef any>
<AttDef any>              ::= AttDef*
event NotationType1$ = completed <NotationType1>
<NotationType1>           ::= <S maybe> '|' <S maybe> Name
event NotationType1_any$ = completed <NotationType1 any>
<NotationType1 any>       ::= <NotationType1>*
event Enumeration1$ = completed <Enumeration1>
<Enumeration1>            ::= <S maybe> '|' <S maybe> Nmtoken
event Enumeration1_any$ = completed <Enumeration1 any>
<Enumeration1 any>        ::= <Enumeration1>*
event ignoreSectContents_any$ = completed <ignoreSectContents any>
<ignoreSectContents any>  ::= ignoreSectContents*
event ignoreSectContents1$ = completed <ignoreSectContents1>
<ignoreSectContents1>     ::= '<![' ignoreSectContents ']]>' Ignore
event ignoreSectContents1_any$ = completed <ignoreSectContents1 any>
<ignoreSectContents1 any> ::= <ignoreSectContents1>*
event VersionInfo_maybe$ = completed <VersionInfo maybe>
<VersionInfo maybe>       ::= VersionInfo
<VersionInfo maybe>       ::=
event TextDecl_maybe$ = completed <TextDecl maybe>
<TextDecl maybe>          ::= TextDecl
<TextDecl maybe>          ::=
event EncName_header$ = completed <EncName header>
<EncName header>          ::= [A-Za-z]
event EncName_trailer$ = completed <EncName trailer>
<EncName trailer>         ::= [A-Za-z0-9._-]
event EncName_trailer_any$ = completed <EncName trailer any>
<EncName trailer any>     ::= <EncName trailer>*

#############################
# For element start detection
#############################
:lexeme ::= STAG pause => before event => ^STAG
STAG                        ~ '<'

##################
# For element jump
##################
ELEMENT                     ~ [\s\S]

################
# XML Exceptions
################
#
# -------------------------------------------------------------
# Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
# -------------------------------------------------------------
#
event Char_minus_sign$ = completed <Char minus sign>
<Char minus sign>       ::= [\x{9}\x{A}\x{D}\x{20}-\x{2C}\x{2E}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u  # '-' is \x{2D}
event Comment_Interior_Unit$ = completed <Comment Interior Unit>
<Comment Interior Unit> ::=     <Char minus sign>
                          | '-' <Char minus sign>
event Comment_Interior$ = completed <Comment Interior>
<Comment Interior>      ::= <Comment Interior Unit>*
#
# -----------------------------------------------------------
# PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
# -----------------------------------------------------------
#
# No need for exception, because '?>' is longer than Char
#
event PI_Exceptioned$ = completed <PI Exceptioned>
<PI Exceptioned>        ::= Char*
#
# ---------------------------------------------------------
# PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
# ---------------------------------------------------------
#
# The following is working, but we want this module to be
# more user-friendly, saying that a PITarget cannot be 'xml':i more explicitly.
# Since we will use events anyway because of SAX support, we add an explicit
# event for PITarget
# <NAMESTARTCHAR>           ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
# <NAMECHAR>                ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
# <NAMECHAR any>            ~ <NAMECHAR>*
# <NAME>                    ~ <NAMESTARTCHAR> <NAMECHAR any>
# <XML>                     ~ [Xx] [Mm] [Ll]
# <PITarget>              ::= <NAME> - <XML>

<NAMESTARTCHAR>           ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
<NAMECHAR>                ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
<NAMECHAR any>            ~ <NAMECHAR>*
:lexeme ::= PITarget pause => after event => PITarget$
<PITarget>                ~ <NAMESTARTCHAR> <NAMECHAR any>

#
# ---------------------------------------
# CData ::= (Char* - (Char* ']]>' Char*)) 
# ---------------------------------------
#
# No need for exception, because ']]>' is longer than Char
#
event CData_Exceptioned$ = completed <CData Exceptioned>
<CData Exceptioned>     ::= Char*
#
# ------------------------------------------------
# Ignore ::= Char+ - (Char+ ('<![' | ']]>') Char+) 
# ------------------------------------------------
#
# Note that we made Ignore not nullable.
# No need for exception, because '<![' and ']]>' are longer than Char
#
event Ignore_Exceptioned$ = completed <Ignore Exceptioned>
<Ignore Exceptioned>    ::= Char+
#
# -------------------------------------------
# CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
# -------------------------------------------
#
# Note that we made CharData not nullable.
# No need for exception, because ']]>' is longer than <CharData Unit>
#
event CharData_Unit$ = completed <CharData Unit>
<CharData Unit>         ::= [^<&]
event CharData_Exceptioned$ = completed <CharData Exceptioned>
<CharData Exceptioned>  ::= <CharData Unit>+
