use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::XML10;
use Carp qw/croak/;
use Data::Section -setup;
use I18N::Charset qw/iana_charset_name/;
use Log::Any '$log', filter => \&_log_filter;
use MarpaX::ESLIF;
use MarpaX::ESLIF::XML::RecognizerInterface;
use MarpaX::ESLIF::XML::ValueInterface::BOM;
use MarpaX::ESLIF::XML::ValueInterface::Decl;
use MarpaX::ESLIF::XML::ValueInterface::Guess;
use MarpaX::ESLIF::XML::ValueInterface;
use Safe::Isa;

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
    my ($self) = @_;

    local $MarpaX::Logger::XML::Logger::Silent = 1;
    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface->new(reader => $self->{reader}, remember => 1, exhaustion => 1);
    my $valueInterface = MarpaX::ESLIF::XML::ValueInterface::BOM->new();

    if ($BOM_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        my ($encoding, $bytes) = @{$valueInterface->getResult};
        #
        # Get data that has to be reinjected
        #
        my $bookkeeping = $recognizerInterface->bookkeeping();
        #
        # ... Minus the number of bytes used by the BOM
        #
        substr($bookkeeping, 0, $bytes, '');
        my $charset = $self->_iana_charset($encoding);

        $log->debugf("Encoding from BOM: %s <=> Charset %s using %d bytes, bookkeeping: %d bytes", $encoding, $charset, $bytes, length($bookkeeping));

        return ($charset, $bookkeeping)
    }

    return (undef, $recognizerInterface->bookkeeping())
}

sub _iana_charset {
    my ($self, $encoding) = @_;

    return undef unless defined($encoding);
    my $charset = iana_charset_name($encoding) || croak "Failed to get charset name from $encoding";

    return $charset
}

sub _charset_from_guess {
    my ($self, $prev_bookkeeping) = @_;

    local $MarpaX::Logger::XML::Logger::Silent = 1;
    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface->new(reader => $self->{reader}, remember => 1, exhaustion => 1, prev_bookkeeping => $prev_bookkeeping);
    my $valueInterface = MarpaX::ESLIF::XML::ValueInterface::Guess->new();

    if ($GUESS_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        my ($encoding, $bytes) = @{$valueInterface->getResult};
        #
        # Get data that has to be reinjected
        #
        my $bookkeeping = $recognizerInterface->bookkeeping();
        my $charset = $self->_iana_charset($encoding);

        $log->debugf("Encoding from guess: %s <=> Charset %s using %d bytes, bookkeeping: %d bytes", $encoding, $charset, $bytes, length($bookkeeping));

        return ($charset, $bookkeeping)
    }

    return (undef, $recognizerInterface->bookkeeping())
}

sub _charset_from_decl {
    my ($self, $prev_bookkeeping, $encoding) = @_;

    local $MarpaX::Logger::XML::Logger::Silent = 1;
    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface->new(reader => $self->{reader}, remember => 1, exhaustion => 1, prev_bookkeeping => $prev_bookkeeping, isCharacterStream => 1, encoding => $encoding);
    my $valueInterface = MarpaX::ESLIF::XML::ValueInterface::Decl->new();

    if ($DECL_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        my $encoding = $valueInterface->getResult;
        #
        # Get data that has to be reinjected
        #
        my $bookkeeping = $recognizerInterface->bookkeeping();
        my $charset = $self->_iana_charset($encoding);

        $log->debugf("Encoding from declaration: %s <=> Charset %s, bookkeeping: %d bytes", $encoding, $charset, length($bookkeeping));

        return ($charset, $bookkeeping)
    }

    return (undef, $recognizerInterface->bookkeeping())
}

sub parse {
    my ($self) = @_;

    my ($charset_from_bom, $charset_from_guess, $charset_from_decl, $bookkeeping);
    #
    # Get encoding from BOM
    #
    ($charset_from_bom, $bookkeeping) = $self->_charset_from_bom;
    #
    # Guess encoding from first bytes
    #
    ($charset_from_guess, $bookkeeping) = $self->_charset_from_guess($bookkeeping);
    #
    # Guess encoding from declaration, this implies charset recognition from MarpaX::ESLIF if no encoding came from BOM or guess
    #
    ($charset_from_decl, $bookkeeping) = $self->_charset_from_decl($bookkeeping, $charset_from_bom // $charset_from_guess);
    #
    # We apply the algorithm "Raw XML charset encoding detection" as per rometools
    # https://rometools.github.io/rome/RssAndAtOMUtilitiEsROMEV0.5AndAboveTutorialsAndArticles/XMLCharsetEncodingDetectionHowRssAndAtOMUtilitiEsROMEHelpsGettingTheRightCharsetEncoding.html
    #
    my $encoding;
    $log->debugf("Charset from BOM: %s, Guess: %s, Declaration: %s", $charset_from_bom, $charset_from_guess, $charset_from_decl);

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

    $log->debugf("Charset used: %s", $encoding);
    #
    # XML itself
    #
    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface->new(reader => $self->{reader}, prev_bookkeeping => $bookkeeping, encoding => $encoding, newline => 1);
    my $valueInterface = MarpaX::ESLIF::XML::ValueInterface->new();
    my $eslifRecognizer = MarpaX::ESLIF::Recognizer->new($XML10_GRAMMAR, $recognizerInterface);
    #
    # We ask for initial events - this must be explicit
    #
    if ($eslifRecognizer->scan()) {
        $self->_manage_events($eslifRecognizer, $recognizerInterface);
        while ($eslifRecognizer->isCanContinue) {
            last unless $eslifRecognizer->resume;
            $self->_manage_events($eslifRecognizer, $recognizerInterface);
        }
    }

    return MarpaX::ESLIF::Value->new($eslifRecognizer, $valueInterface)->value()
}

sub _manage_events {
    my ($self, $eslifRecognizer, $recognizerInterface) = @_;
    #
    # Remember, events are always sorted like this:
    # - completion, then
    # - nullable, then
    # - prediction
    #
    # This mean that, if a BOM is recognized, its event
    # is guaranteed to come before prolog prediction event
    #
    foreach (@{$eslifRecognizer->events()}) {
        my $event = $_->{event};
        my $symbol = $_->{symbol};
        if ($event =~ /^lexeme/) {
            my $value = $eslifRecognizer->lexemeLastPause($symbol);
            if ($log->is_trace) {
                $log->tracef("Lexeme <%s>, length %d, value: %s", $symbol, length($value), $value);
            } else {
                $log->debugf("Lexeme <%s>, length %d", $symbol, length($value));
            }
        } else {
            $log->debugf("Event %s on symbol %s", $event, $symbol);
        }
    }
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
# Unusual ordering is not considered
#
Guess ::= [\x{00}] [\x{00}] [\x{00}] [\x{3C}] action => UTF_32BE
        | [\x{3C}] [\x{00}] [\x{00}] [\x{00}] action => UTF_32_E
        | [\x{00}] [\x{3C}] [\x{00}] [\x{3F}] action => UTF_16BE
        | [\x{3C}] [\x{00}] [\x{3F}] [\x{00}] action => UTF_16LE
        | [\x{3C}] [\x{3F}] [\x{78}] [\x{6D}] action => UTF_8
        | [\x{4C}] [\x{6F}] [\x{A7}] [\x{94}] action => EBCDIC

__[ XML 1.0 ]__
#
# From https://www.w3.org/TR/REC-xml (5th edition)
#
# Take care, original has several ambiguities:
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0000 (applied)
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001 (applied)
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002 (applied)
#
document           ::= prolog element <Misc any>
Char               ::= [\x{9}\x{A}\x{D}\x{20}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u
S1                 ::= [\x{20}\x{9}\x{D}\x{A}]:u
S                  ::= S1+
NameStartChar      ::= [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
NameChar           ::= [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
Name               ::= NameStartChar <NameChar any>
Names              ::= Name+ separator => [\x{20}]:u
Nmtoken            ::= NameChar+
Nmtokens           ::= Nmtoken+ separator => [\x{20}]:u
EntityValue        ::= '"' <EntityValue1 any>   '"' | "'" <EntityValue2 any>   "'"
AttValue           ::= '"' <AttValue1 any>      '"' | "'" <AttValue2 any>      "'"
SystemLiteral      ::= '"' <SystemLiteral1 any> '"' | "'" <SystemLiteral2 any> "'"
PubidLiteral       ::= '"' <PubidChar1 any>     '"' | "'" <PubidChar2 any>     "'"
PubidChar          ::= [\x{20}\x{D}\x{A}a-zA-Z0-9\-'()+,./:=?;!*#@$_%]
CharData           ::= <CharData ok> - <CharData exception>
#
# https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0000
#
# We make not nullable
#
# CharData           ::=                                                         # Because a lexeme can never be a nullable
Comment            ::= '<!--' <Comment1 any> '-->'
PI                 ::= '<?' PITarget         '?>'
                     | '<?' PITarget S <PI1> '?>'
PITarget           ::= <PITarget ok> - <PITarget exception>
CDSect             ::= CDStart CData CDEnd
CDStart            ::= '<![CDATA['
CData              ::= <CData ok> - <CData exception>
CData              ::= # Because a lexeme can never be a nullable
CDEnd              ::= ']]>'
prolog             ::= <XMLDecl maybe> <Misc any>
                     | <XMLDecl maybe> <Misc any> doctypedecl <Misc any>
XMLDecl            ::= '<?xml' VersionInfo <EncodingDecl maybe> <SDDecl maybe> <S maybe> '?>' ## Decl_action => ::copy[2]
VersionInfo        ::= S 'version' Eq "'" VersionNum "'"
                     | S 'version' Eq '"' VersionNum '"'
Eq                 ::= <S maybe> '=' <S maybe>
VersionNum         ::= '1.' <digit many>
#
# https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002
#
# Use S1 instead of S in Misc
#
Misc               ::= Comment | PI | S1
doctypedecl        ::= '<!DOCTYPE' S Name              <S maybe>                             '>'
                     | '<!DOCTYPE' S Name              <S maybe> '[' intSubset ']' <S maybe> '>'
                     | '<!DOCTYPE' S Name S ExternalID <S maybe>                             '>'
                     | '<!DOCTYPE' S Name S ExternalID <S maybe> '[' intSubset ']' <S maybe> '>'
#
# https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001
#
# Change S in DeclSep to S1
#
DeclSep            ::= PEReference | S1
intSubset          ::= <intSubset1 any>
markupdecl         ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
extSubset          ::=          extSubsetDecl
                     | TextDecl extSubsetDecl
extSubsetDecl      ::= <extSubsetDecl1 any>
SDDecl             ::= S 'standalone' Eq "'" <yes or no> "'"
                     | S 'standalone' Eq '"' <yes or no> '"'
element            ::= EmptyElemTag
                     | STag content ETag 
STag               ::= '<' Name <STag1 any> <S maybe> '>'
Attribute          ::= Name Eq AttValue 
ETag               ::= '</' Name <S maybe> '>'
content            ::= <CharData maybe> <content1 any>
EmptyElemTag       ::= '<' Name <EmptyElemTag1 any> <S maybe> '/>'
elementdecl        ::= '<!ELEMENT' S Name S contentspec <S maybe> '>'
contentspec        ::= 'EMPTY' | 'ANY' | Mixed | children 
children           ::= <choice or seq> <sequence maybe>
cp                 ::= <Name or choice or seq> <sequence maybe>
choice             ::= '(' <S maybe> cp <choice1 many> <S maybe> ')'
seq                ::= '(' <S maybe> cp <seq1 any>     <S maybe> ')'
Mixed              ::= '(' <S maybe> '#PCDATA' <Mixed1 any> <S maybe> ')*'
                     | '(' <S maybe> '#PCDATA'              <S maybe> ')'
AttlistDecl        ::= '<!ATTLIST' S Name <AttDef any> <S maybe> '>'
AttDef             ::= S Name S AttType S DefaultDecl 
AttType            ::= StringType | TokenizedType | EnumeratedType
StringType         ::= 'CDATA'
TokenizedType      ::= 'ID'| 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
EnumeratedType     ::= NotationType | Enumeration
NotationType       ::= 'NOTATION' S '(' <S maybe> Name    <NotationType1 any> <S maybe> ')'
Enumeration        ::=              '(' <S maybe> Nmtoken <Enumeration1 any>  <S maybe> ')'
DefaultDecl        ::= '#REQUIRED'
                     | '#IMPLIED'
                     |            AttValue
                     | '#FIXED' S AttValue
conditionalSect    ::= includeSect | ignoreSect
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

ignoreSect         ::= '<![' <S maybe> 'IGNORE' <S maybe> '[' <ignoreSectContents any> ']]>'
                     | '<![' <S maybe> 'IGNORE' <S maybe> '['                          ']]>'
ignoreSectContents ::= Ignore <ignoreSectContents1 any>
Ignore             ::= <Ignore ok> - <Ignore exception>

CharRef            ::= '&#' <digit many> ';'
                     | '&#x' <hexdigit many> ';'
Reference          ::= EntityRef | CharRef
EntityRef          ::= '&' Name ';'
PEReference        ::= '%' Name ';'
EntityDecl         ::= GEDecl | PEDecl
GEDecl             ::= '<!ENTITY' S Name S EntityDef <S maybe> '>'
PEDecl             ::= '<!ENTITY' S '%' S Name S PEDef <S maybe> '>'
EntityDef          ::= EntityValue
                     | ExternalID
                     | ExternalID NDataDecl
PEDef              ::= EntityValue | ExternalID 
ExternalID         ::= 'SYSTEM' S SystemLiteral
                     | 'PUBLIC' S PubidLiteral S SystemLiteral
NDataDecl          ::= S 'NDATA' S Name 
TextDecl           ::= '<?xml' <VersionInfo maybe> EncodingDecl <S maybe> '?>'
extParsedEnt       ::= <TextDecl maybe> content 
EncodingDecl       ::= S 'encoding' Eq '"' EncName '"'                               ## Decl_action => ::copy[4]
                     | S 'encoding' Eq "'" EncName "'"                               ## Decl_action => ::copy[4]
EncName            ::= <EncName header> <EncName trailer any>
NotationDecl       ::= '<!NOTATION' S Name S ExternalID <S maybe> '>' 
                     | '<!NOTATION' S Name S PublicID   <S maybe> '>' 
PublicID           ::= 'PUBLIC' S PubidLiteral 

<Misc any>                ::= Misc*
<NameChar any>            ::= NameChar*
<EntityValue1>            ::= [^%&"] | PEReference | Reference
<EntityValue2>            ::= [^%&'] | PEReference | Reference
<EntityValue1 any>        ::= <EntityValue1>*
<EntityValue2 any>        ::= <EntityValue2>*
<AttValue1>               ::= [^<&"] | Reference
<AttValue2>               ::= [^<&'] | Reference
<AttValue1 any>           ::= <AttValue1>*
<AttValue2 any>           ::= <AttValue2>*
<SystemLiteral1>          ::= [^"]
<SystemLiteral2>          ::= [^']
<SystemLiteral1 any>      ::= <SystemLiteral1>*
<SystemLiteral2 any>      ::= <SystemLiteral2>*
<PubidChar1 any>          ::= PubidChar*
<PubidChar2>              ::= [\x{20}\x{D}\x{A}a-zA-Z0-9\-()+,./:=?;!*#@$_%]  # Same as PubidChar but without '
<PubidChar2 any>          ::= <PubidChar2>*
<Char without minus>      ::= [\x{9}\x{A}\x{D}\x{20}-\x{2C}\x{2E}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u # '-' is \x{2D}
<Comment1>                ::=     [\x{9}\x{A}\x{D}\x{20}-\x{2C}\x{2E}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u # '-' is \x{2D}
                            | '-' [\x{9}\x{A}\x{D}\x{20}-\x{2C}\x{2E}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u # '-' is \x{2D}
<Comment1 any>            ::= <Comment1>*
<PI1>                     ::= <PI1 ok> - <PI1 exception>
<PI1>                     ::= # Because a lexeme can never be a nullable
<XMLDecl maybe>           ::= XMLDecl
<XMLDecl maybe>           ::=
<EncodingDecl maybe>      ::= EncodingDecl ## Decl_action => ::shift
<EncodingDecl maybe>      ::=
<SDDecl maybe>            ::= SDDecl
<SDDecl maybe>            ::= 
<S maybe>                 ::= S
<S maybe>                 ::= 
<digit>                   ::= [0-9]
<digit many>              ::= <digit>+
<hexdigit>                ::= [0-9a-fA-F]
<hexdigit many>           ::= <hexdigit>+
<intSubset1>              ::= markupdecl | DeclSep
<intSubset1 any>          ::= <intSubset1>*
<extSubsetDecl1>          ::= markupdecl | conditionalSect | DeclSep
<extSubsetDecl1 any>      ::= <extSubsetDecl1>*
<yes or no>               ::= 'yes' | 'no'
<STag1>                   ::= S Attribute
<STag1 any>               ::= <STag1>*
<CharData maybe>          ::= CharData
<CharData maybe>          ::=
<content1>                ::= element   <CharData maybe>
                            | Reference <CharData maybe>
                            | CDSect    <CharData maybe>
                            | PI        <CharData maybe>
                            | Comment   <CharData maybe>
<content1 any>            ::= <content1>*
<EmptyElemTag1>           ::= S Attribute
<EmptyElemTag1 any>       ::= <EmptyElemTag1>*
<choice or seq>           ::= choice | seq
<sequence>                ::= '?' | '*' | '+'
<sequence maybe>          ::= <sequence>
<sequence maybe>          ::=
<Name or choice or seq>   ::= Name | choice | seq
<choice1>                 ::= <S maybe> '|' <S maybe> cp
<choice1 many>            ::= <choice1>+
<seq1>                    ::= <S maybe> ',' <S maybe> cp
<seq1 any>                ::= <seq1>*
<Mixed1>                  ::= <S maybe> '|' <S maybe> Name
<Mixed1 any>              ::= <Mixed1>*
<AttDef any>              ::= AttDef*
<NotationType1>           ::= <S maybe> '|' <S maybe> Name
<NotationType1 any>       ::= <NotationType1>*
<Enumeration1>            ::= <S maybe> '|' <S maybe> Nmtoken
<Enumeration1 any>        ::= <Enumeration1>*
<ignoreSectContents any>  ::= ignoreSectContents*
<ignoreSectContents1>     ::= '<![' ignoreSectContents ']]>' Ignore
<ignoreSectContents1 any> ::= <ignoreSectContents1>*
<VersionInfo maybe>       ::= VersionInfo
<VersionInfo maybe>       ::=
<TextDecl maybe>          ::= TextDecl
<TextDecl maybe>          ::=
<EncName header>          ::= [A-Za-z]
<EncName trailer>         ::= [A-Za-z0-9._-]
<EncName trailer any>     ::= <EncName trailer>*

<_CHARDATA>            ~ [^<&]
<_CHAR>                ~ [\x{9}\x{A}\x{D}\x{20}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u
<_CHARDATA any>        ~ <_CHARDATA>*
<_CHAR any>            ~ <_CHAR>*
<_CHARDATA EXCEPTION>  ~ <_CHARDATA any> ']]>' <_CHARDATA any>
<_PI EXCEPTION>        ~ <_CHAR any> '?>' <_CHAR any>
<_CDATA EXCEPTION>     ~ <_CHAR any> ']]>' <_CHAR any>
<_IGNORE EXCEPTION>    ~ <_CHAR any> '<![' <_CHAR any>
                       | <_CHAR any> ']]>' <_CHAR any>
<_NAMESTARTCHAR>       ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
<_NAMECHAR>            ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
<_NAMECHAR any>        ~ <_NAMECHAR>*
<_NAME>                ~ <_NAMESTARTCHAR> <_NAMECHAR any>
<_PITARGET EXCEPTION>  ~ [Xx] [Mm] [Ll]

:lexeme ::= <CharData ok> pause => after event => lexemeCharData$
<CharData ok>          ~ <_CHARDATA any>
<CharData exception>   ~ <_CHARDATA EXCEPTION>

:lexeme ::= <PITarget ok> pause => after event => lexemePITargetok$
<PITarget ok>          ~ <_NAME>
<PITarget exception>   ~ <_PITARGET EXCEPTION>

:lexeme ::= <CData ok> pause => after event => lexemeCDataok$
<CData ok>             ~ <_CHAR any>
<CData exception>      ~ <_CDATA EXCEPTION>

:lexeme ::= <Ignore ok> pause => after event => lexemeIgnoreok$
<Ignore ok>            ~ <_CHAR any>
<Ignore exception>     ~ <_IGNORE EXCEPTION>

:lexeme ::= <PI1 ok> pause => after event => lexemePI1ok$
<PI1 ok>               ~ <_CHAR any>
<PI1 exception>        ~ <_PI EXCEPTION>
