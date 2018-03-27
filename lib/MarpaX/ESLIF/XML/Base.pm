use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Base;
use Carp qw/croak/;
use Class::Tiny qw//, {
    event_callback  => sub { {} }
};
use Data::Section -setup;
use I18N::Charset qw/iana_charset_name/;
use Log::Any qw/$log/, filter => \&_log_filter;
use MarpaX::ESLIF 2.0.42 qw//; # isCanContinue normalization
use MarpaX::ESLIF::XML::Base::Recognizer::Interface qw//;
use MarpaX::ESLIF::XML::Base::Value::Interface qw//;
use MarpaX::ESLIF::XML::Base::Value::Interface::BOM qw//;
use MarpaX::ESLIF::XML::Base::Value::Interface::Decl qw//;
use MarpaX::ESLIF::XML::Base::Value::Interface::Guess qw//;
use Role::Tiny qw/requires/;

requires qw/reader decl_grammar document_grammar element_grammar extParsedEnt_grammar/;

# ABSTRACT: XML parser role

# VERSION

# AUTHORITY

=head1 DESCRIPTION

XML parser role. Requires five methods for consumption:

=over

=item reader

A method that returns new data, undef when EOF is reached.

=item decl_grammar

A method that returns a MarpaX::ESLIF::Grammar instance of the XMLDecl grammar, i.e.:

  my $decl_grammar = $self->decl_grammar(); # MarpaX::ESLIF::Grammar instance

This grammar must have actions already set up so that its valuation return the encoding part of the XML stream. For example, when applied on a stream that is starting with:

  <?xml version="1.0" encoding="ISO-8859-1"?>

the valuation of the grammar will return the string C<ISO-8859-1>. This grammar will be executed using C<MarpaX::ESLIF::Grammar::parse> method, this mean that no interaction with userspace will be possible. Valuation will return C<undef> if there is no encoding declaration or if the stream parse fails.

=item document_grammar

A method that returns a MarpaX::ESLIF::Grammar instance of the XML grammar starting at document symbol, i.e.:

  my $document_grammar = $self->document_grammar(); # MarpaX::ESLIF::Grammar instance

=item element_grammar

A method that returns a MarpaX::ESLIF::Grammar instance of the XML grammar where the start is an element, i.e.:

  my $element_grammar = $self->element_grammar(); # MarpaX::ESLIF::Grammar instance

This grammar contains the full XML specification like in C<document_grammar>, except that the start symbol is the I<element>. The reasoning behind this additional grammar is that I<element> is the pivot of any XML, which recurses on that symbol.

Although this is technically possible to parse a full XML stream using only C<document_grammar>, memory exhaustion could happen if the stream is very large, because at the very low level, a Marpa parser always remembers at least that some information matched, even without the associated values. In order to be streaming compatible, a new parsing is done at every element node.

=item extParsedEnt_grammar

A method that returns a MarpaX::ESLIF::Grammar instance of the XML grammar starting at extParsedEnt symbol, i.e.:

  my $extParsedEnt_grammar = $self->extParsedEnt_grammar(); # MarpaX::ESLIF::Grammar instance

=item event_callback

A method that will be called everytime the parsing is interruped with an event, with the the following parameters:

=over

=item A reference to an anonymous hash containing:

=over

=item type

The type of an event, guaranteed to be a C<MarpaX::ESLIF::Event::Type> value

=item symbol

The name of the corresponding symbol. Can be C<undef> if this is an exhaustion event.

=item event

The name of the event. Can be the special value C<'exhausted'> if this is an exhaustion event.

=back

=item The instance of the recognizer used at the time of this event.

=back

For example:

  $self->event_callback({type => ..., symbol => ..., event => ...}, $recognizerInstance);

=back

=cut

# -----------------
# Main ESLIF object
# -----------------
my $ESLIF = MarpaX::ESLIF->new($log);
sub eslif {
    return $ESLIF
}
# -----------------------------------------------
# Grammar for BOM detection using the first bytes
# -----------------------------------------------
my $BOM_SOURCE  = ${__PACKAGE__->section_data('BOM')};
my $BOM_GRAMMAR = MarpaX::ESLIF::Grammar->new($ESLIF, $BOM_SOURCE);

# ------------------------------------------
# Grammar for encoding guess using the bytes
# ------------------------------------------
my $GUESS_SOURCE  = ${__PACKAGE__->section_data('Guess')};
my $GUESS_GRAMMAR = MarpaX::ESLIF::Grammar->new($ESLIF, $GUESS_SOURCE);

# -------------------------------------
# Shared BNF between all version of XML
# -------------------------------------
my $SHARED_SOURCE  = ${__PACKAGE__->section_data('XML')};
sub shared_source {
    return $SHARED_SOURCE
}

# =============================================================================
# _log_filter
#
# Log filtering
# =============================================================================
sub _log_filter {
    my ($category, $level, $msg) = @_;

    return if $MarpaX::ESLIF::XML::Silent;
    return $msg
}

# =============================================================================
# _iana_charset
#
# Returns a hopefully IANA compatible charset name
# =============================================================================
sub _iana_charset {
    my ($self, $origcharset) = @_;

    my $charset;
    if (defined($origcharset)) {
        if (lc($origcharset) eq 'unicode') {
            #
            # Common pitfall
            #
            $log->warnf('Encoding "%s" interpreted as "UTF-16"', $origcharset);
            $charset = 'UTF-16'
        } else {
            #
            # This should never fail.
            #
            $charset = uc(iana_charset_name($origcharset)) ||
                       croak "Failed to get charset name from $origcharset";
            #
            # We always use the uppercased version so that _merge_charsets()
            # does not have to take care of case sensitivity
            #
            $charset = uc($charset)
        }
    }

    return $charset
}

# =============================================================================
# _charset_from_bom
#
# Get charset from BOM, eventually
# =============================================================================
sub _charset_from_bom {
    my ($self, $recognizerInterface) = @_;

    local $MarpaX::ESLIF::XML::Silent = 1;
    my $valueInterface = MarpaX::ESLIF::XML::Base::Value::Interface::BOM->new();

    my ($charset, $bytes);
    if ($BOM_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        #
        # Values are already IANA compatible and uppercased
        #
        ($charset, $bytes) = @{$valueInterface->getResult};
        #
        # Get data that has to be reinjected
        #
        my $bookkeeping = $recognizerInterface->bookkeeping();
        #
        # ... Minus the number of bytes used by the BOM: they are formally NOT
        # part of the XML grammar
        #
        substr($bookkeeping, 0, $bytes, '');
        $recognizerInterface->bookkeeping($bookkeeping);
    }

    return $charset
}

# =============================================================================
# _charset_from_guess
#
# Get charset from first bytes, eventually
# =============================================================================
sub _charset_from_guess {
    my ($self, $recognizerInterface) = @_;

    local $MarpaX::ESLIF::XML::Silent = 1;
    my $valueInterface = MarpaX::ESLIF::XML::Base::Value::Interface::Guess->new();

    my $charset;
    if ($GUESS_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        #
        # Values are already IANA compatible and uppercased
        #
        $charset = $valueInterface->getResult
    }

    return $charset
}

# =============================================================================
# _charset_from_decl
#
# Get charset from XMLDecl, eventually
# =============================================================================
sub _charset_from_decl {
    my ($self, $recognizerInterface) = @_;

    local $MarpaX::ESLIF::XML::Silent = 1;
    my $valueInterface = MarpaX::ESLIF::XML::Base::Value::Interface::Decl->new();

    my $charset;
    if ($self->decl_grammar->parse($recognizerInterface, $valueInterface)) {
        #
        # In case this is an alias, uppercased IANA version is preferred
        #
        $charset = $self->_iana_charset($valueInterface->getResult)
    }

    return $charset
}

# =============================================================================
# This is the "Raw XML charset encoding detection" as per rometools,
# extended to UTF-32.
# =============================================================================
#
# C.f. https://rometools.github.io/rome/RssAndAtOMUtilitiEsROMEV0.5AndAboveTutorialsAndArticles/XMLCharsetEncodingDetectionHowRssAndAtOMUtilitiEsROMEHelpsGettingTheRightCharsetEncoding.html
#
# I disagree with them nevertheless in the following case:
#
# if BOMEnc is NULL
#   if XMLGuessEnc is NULL or XMLEnc is NULL
#     encoding is 'UTF-8'                                                 [1.0]
#   endif
# endif
#
#
# Because if XMLEnc is set, it should be used, then XMLGuessEnc, then 'UTF-8.
# ======================================================================================
sub _merge_charsets {
    my ($self, $charset_from_bom, $charset_from_guess, $charset_from_decl) = @_;

    $log->tracef("Merging encodings from BOM: %s, Guess: %s and Declaration: %s",
                 $charset_from_bom,
                 $charset_from_guess,
                 $charset_from_decl);
    my $charset;
    if (! defined($charset_from_bom)) {
        if (! defined($charset_from_guess) || ! defined($charset_from_decl)) {
            $charset = $charset_from_decl // $charset_from_guess // 'UTF-8'
        } else {
            if (($charset_from_decl eq 'UTF-16') && ($charset_from_guess eq 'UTF-16BE' || $charset_from_guess eq 'UTF-16LE')) {
                $charset =  $charset_from_guess
            } elsif (($charset_from_decl eq 'UTF-32') && ($charset_from_guess eq 'UTF-32BE' || $charset_from_guess eq 'UTF-32LE')) {
                $charset =  $charset_from_guess
            } else {
                $charset = $charset_from_decl
            }
        }
    } else {
        if ($charset_from_bom eq 'UTF-8') {
            if (defined($charset_from_guess) && $charset_from_guess ne 'UTF-8') {
                croak "Encoding mismatch between BOM $charset_from_bom and guess $charset_from_guess"
            }
            if (defined($charset_from_decl) && $charset_from_decl ne 'UTF-8') {
                croak "Encoding mismatch between BOM $charset_from_bom and declaration $charset_from_decl"
            }
            $charset = 'UTF-8'
        } else {
            if ($charset_from_bom eq 'UTF-16BE' or $charset_from_bom eq 'UTF-16LE') {
                if (defined($charset_from_guess) && $charset_from_guess ne $charset_from_bom) {
                    croak "Encoding mismatch between BOM $charset_from_bom and guess $charset_from_guess"
                }
                if (defined($charset_from_decl) && ($charset_from_decl ne 'UTF-16' and $charset_from_decl ne $charset_from_bom)) {
                    croak "Encoding mismatch between BOM $charset_from_bom and declaration $charset_from_decl"
                }
                $charset = $charset_from_bom
            } elsif ($charset_from_bom eq 'UTF-32BE' or $charset_from_bom eq 'UTF-32LE') {
                if (defined($charset_from_guess) && $charset_from_guess ne $charset_from_bom) {
                    croak "Encoding mismatch between BOM $charset_from_bom and guess $charset_from_guess"
                }
                if (defined($charset_from_decl) && ($charset_from_decl ne 'UTF-32' and $charset_from_decl ne $charset_from_bom)) {
                    croak "Encoding mismatch between BOM $charset_from_bom and declaration $charset_from_decl"
                }
                $charset = $charset_from_bom
            } else {
                croak 'Encoding setup failed'
            }
        }
    }

    $log->tracef("Charset from merge is: %s", $charset);
    return $charset
}

# ======================================================================================
# Parses XML
# ======================================================================================
sub parse {
    my ($self) = @_;

    # ------------------
    # Charset from BOM ?
    # ------------------
    my $recognizerInterface = MarpaX::ESLIF::XML::Base::Recognizer::Interface->new(
        isCharacterStream => 0,
        isWithExhaustion  => 1,
        reader            => $self->reader,
        remember          => 1
        );
    my $charset_from_bom = $self->_charset_from_bom($recognizerInterface);
    $log->tracef(
        "Encoding from BOM: %s, bookkeeping: %d bytes",
        $charset_from_bom,
        bytes::length($recognizerInterface->bookkeeping));

    # --------------------
    # Charset from guess ?
    # --------------------
    $recognizerInterface = MarpaX::ESLIF::XML::Base::Recognizer::Interface->new(
        isWithExhaustion => 1,
        reader           => $self->reader,
        remember         => 1,
        initial_data     => $recognizerInterface->bookkeeping,
        initial_eof      => $recognizerInterface->isEof
        );
    my $charset_from_guess = $self->_charset_from_guess($recognizerInterface);
    $log->tracef(
        "Encoding from Guess: %s, bookkeeping: %d bytes",
        $charset_from_guess,
        bytes::length($recognizerInterface->bookkeeping));

    # --------------------------
    # Charset from declaration ?
    # --------------------------
    $recognizerInterface = MarpaX::ESLIF::XML::Base::Recognizer::Interface->new(
        isWithExhaustion => 1,
        reader           => $self->reader,
        remember         => 1,
        initial_data     => $recognizerInterface->bookkeeping,
        initial_eof      => $recognizerInterface->isEof);
    my $charset_from_decl = $self->_charset_from_decl($recognizerInterface);
    $log->tracef(
        "Encoding from Declaration: %s, bookkeeping: %d bytes",
        $charset_from_decl,
        bytes::length($recognizerInterface->bookkeeping));

    # --------------
    # Merge charsets
    # --------------
    my $charset = $self->_merge_charsets(
        $charset_from_bom,
        $charset_from_guess,
        $charset_from_decl);
    
    # ----------
    # XML itself
    # ----------
    $recognizerInterface = MarpaX::ESLIF::XML::Base::Recognizer::Interface->new(
        reader       => $self->reader,
        initial_data => $recognizerInterface->bookkeeping,
        initial_eof  => $recognizerInterface->isEof,
        encoding     => $charset
        );
    my $eslifRecognizer = MarpaX::ESLIF::Recognizer->new(
        $self->document_grammar,
        $recognizerInterface
        );
    #
    # Scan XML
    #
    return $self->_parse(
        $eslifRecognizer,
        0 # level
        )
}

# ======================================================================================
# _parse
#
# XML parse implementation that recurses on element
# ======================================================================================
sub _parse {
    no warnings 'recursion';
    my ($self, $currentRecognizer, $level) = @_;

    return 0 unless $currentRecognizer->scan();
    return 0 unless $self->_manage_events($currentRecognizer, $level);
    if ($currentRecognizer->isCanContinue) {
        do {
            return 0 unless $currentRecognizer->resume;
            my $rcb = $self->_manage_events($currentRecognizer, $level);
            return 0 unless $rcb;
            return 1 if ($rcb < 0)
        } while ($currentRecognizer->isCanContinue)
    }

    return 1
}

# ======================================================================================
# _manage_events
#
# XML events manager. Will pile up as many recognizers as there are composite elements
# ======================================================================================
sub _manage_events {
    my ($self, $currentRecognizer, $level) = @_;

    foreach (@{$currentRecognizer->events()}) {
        
        if ($_->{event} eq '^ELEMENT_START') {
            #
            # Create an element recognizer
            #
            my $elementRecognizer = $currentRecognizer->newFrom($self->element_grammar);
            #
            # Enable element end events
            #
            $elementRecognizer->eventOnOff('ELEMENT_END1', [ MarpaX::ESLIF::Event::Type->MARPAESLIF_EVENTTYPE_BEFORE ], 1);
            $elementRecognizer->eventOnOff('ELEMENT_END2', [ MarpaX::ESLIF::Event::Type->MARPAESLIF_EVENTTYPE_BEFORE ], 1);
            #
            # Inject the ELEMENT_START lexeme.
            #
            return 0 unless $elementRecognizer->lexemeRead('ELEMENT_START', '<', 1); # In UTF-8 '<' is one byte
            #
            # Call for the element parsing
            #
            return 0 unless $self->_parse($elementRecognizer, $level + 1);
            #
            # Push the ELEMENT_VALUE
            #
            return $currentRecognizer->lexemeRead('ELEMENT_VALUE', undef, 0)
        } elsif ($_->{event} eq '^ELEMENT_END1' || $_->{event} eq '^ELEMENT_END2') {
            #
            # Allow exhaustion
            #
            $currentRecognizer->set_exhausted_flag(1);
            #
            # Push the lexeme
            #
            my $symbol = $_->{symbol};
            my $utf8Length = $symbol eq 'ELEMENT_END1' ? 1 : 2; # '>' or '/>'
            return $currentRecognizer->lexemeRead($symbol, undef, $utf8Length)
        } else {
            #
            # Should never happen
            #
            return 0
        }
    }

    return 1
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
# Guess encoding should not return UTF-8, there are two many
# encodings that looks like UTF-8 and will fail later
#
Guess ::= [\x{00}] [\x{00}] [\x{00}] [\x{3C}] action => UTF_32BE # '<'
        | [\x{3C}] [\x{00}] [\x{00}] [\x{00}] action => UTF_32LE # '<'
        | [\x{00}] [\x{3C}] [\x{00}] [\x{3F}] action => UTF_16BE # '<?'
        | [\x{3C}] [\x{00}] [\x{3F}] [\x{00}] action => UTF_16LE # '<?'

__[ XML ]__
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

# event document$ = completed document
document           ::= prolog element <Misc any>
# event Char$ = completed Char
Char               ::= [\x{9}\x{A}\x{D}\x{20}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u name => Char
# event S1$ = completed S1
S1                 ::= [\x{20}\x{9}\x{D}\x{A}]
# event S$ = completed S
S                  ::= S1+
# event NameStartChar$ = completed NameStartChar
NameStartChar      ::= [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
# event NameChar$ = completed NameChar
NameChar           ::= [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
# event Name$ = completed Name
# Name               ::= NameStartChar <NameChar any>
Name               ::= <NAME>
# event Names$ = completed Names
Names              ::= Name+ separator => [\x{20}]
# event Nmtoken$ = completed Nmtoken
Nmtoken            ::= NameChar+
# event Nmtokens$ = completed Nmtokens
Nmtokens           ::= Nmtoken+ separator => [\x{20}]
# event EntityValue$ = completed EntityValue
EntityValue        ::= '"' <EntityValue1 any>   '"'
                     | "'" <EntityValue2 any>   "'"
# event AttValue$ = completed AttValue
AttValue           ::= '"' <AttValue1 any>      '"'
                     | "'" <AttValue2 any>      "'"
# event SystemLiteral$ = completed SystemLiteral
SystemLiteral      ::= '"' <SystemLiteral1 any> '"'
                     | "'" <SystemLiteral2 any> "'"
# event PubidLiteral$ = completed PubidLiteral
PubidLiteral       ::= '"' <PubidChar1 any>     '"'
                     | "'" <PubidChar2 any>     "'"
# event PubidChar$ = completed PubidChar
PubidChar          ::= [\x{20}\x{D}\x{A}a-zA-Z0-9\-'()+,./:=?;!*#@$_%]
# event CharData$ = completed CharData
CharData           ::= <CharData Exceptioned>
# event Comment$ = completed Comment
Comment            ::= '<!--' <Comment Interior> '-->'
# event PI$ = completed PI
PI                 ::= '<?' PITarget                    '?>'
                     | '<?' PITarget S <PI Exceptioned> '?>'
# event CDSect$ = completed CDSect
CDSect             ::= CDStart CData CDEnd
# event CDStart$ = completed CDStart
CDStart            ::= '<![CDATA['
# event CData$ = completed CData
CData              ::= <CData Exceptioned>
# event CDEnd$ = completed CDEnd
CDEnd              ::= ']]>'
# event prolog$ = completed prolog
prolog             ::= <XMLDecl maybe> <Misc any>
                     | <XMLDecl maybe> <Misc any> doctypedecl <Misc any>
# event XMLDecl$ = completed XMLDecl
#
# Note: it is important to split '<?xml' into '<?' 'xml' because of PI whose defintion is: '<?' PITarget
#
XMLDecl            ::= '<?' 'xml' VersionInfo <EncodingDecl maybe> <SDDecl maybe> <S maybe> '?>' ## Decl_action => ::copy[3]
# event VersionInfo$ = completed VersionInfo
VersionInfo        ::= S 'version' Eq "'" VersionNum "'"
                     | S 'version' Eq '"' VersionNum '"'
# event Eq$ = completed Eq
Eq                 ::= <S maybe> '=' <S maybe>
# event VersionNum$ = completed VersionNum
VersionNum         ::= '1.' <digit many>
#
# https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002
#
# Use S1 instead of S in Misc
#
# event Misc$ = completed Misc
Misc               ::= Comment
                     | PI
                     | S1
# event doctypedecl$ = completed doctypedecl
doctypedecl        ::= '<!DOCTYPE' S Name              <S maybe>                             '>'
                     | '<!DOCTYPE' S Name              <S maybe> '[' intSubset ']' <S maybe> '>'
                     | '<!DOCTYPE' S Name S ExternalID <S maybe>                             '>'
                     | '<!DOCTYPE' S Name S ExternalID <S maybe> '[' intSubset ']' <S maybe> '>'
#
# https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001
#
# Change S in DeclSep to S1
#
# event DeclSep$ = completed DeclSep
DeclSep            ::= PEReference
                     | S1
# event intSubset$ = completed intSubset
intSubset          ::= <intSubset1 any>
# event markupdecl$ = completed markupdecl
markupdecl         ::= elementdecl
                     | AttlistDecl
                     | EntityDecl
                     | NotationDecl
                     | PI
                     | Comment
# event extSubset$ = completed extSubset
extSubset          ::=          extSubsetDecl
                     | TextDecl extSubsetDecl
# event extSubsetDecl$ = completed extSubsetDecl
extSubsetDecl      ::= <extSubsetDecl1 any>
# event SDDecl$ = completed SDDecl
SDDecl             ::= S 'standalone' Eq "'" <yes or no> "'"
                     | S 'standalone' Eq '"' <yes or no> '"'
element            ::= EmptyElemTag
                     | STag content ETag
                     | ELEMENT_VALUE
# event STag$ = completed STag
STag               ::= ELEMENT_START Name <STag1 any> <S maybe> '>'
# event Attribute$ = completed Attribute
Attribute          ::= Name Eq AttValue
# event ETag$ = completed ETag
ETag               ::= '</' Name <S maybe> ELEMENT_END1
# event content$ = completed content
content            ::= <CharData maybe> <content1 any>
# event EmptyElemTag$ = completed EmptyElemTag
EmptyElemTag       ::= ELEMENT_START Name <EmptyElemTag1 any> <S maybe> ELEMENT_END2
# event elementdecl$ = completed elementdecl
elementdecl        ::= '<!ELEMENT' S Name S contentspec <S maybe> '>'
# event contentspec$ = completed contentspec
contentspec        ::= 'EMPTY' | 'ANY' | Mixed | children
# event children$ = completed children
children           ::= <choice or seq> <sequence maybe>
# event cp$ = completed cp
cp                 ::= <Name or choice or seq> <sequence maybe>
# event choice$ = completed choice
choice             ::= '(' <S maybe> cp <choice1 many> <S maybe> ')'
# event seq$ = completed seq
seq                ::= '(' <S maybe> cp <seq1 any>     <S maybe> ')'
# event Mixed$ = completed Mixed
Mixed              ::= '(' <S maybe> '#PCDATA' <Mixed1 any> <S maybe> ')*'
                     | '(' <S maybe> '#PCDATA'              <S maybe> ')'
# event AttlistDecl$ = completed AttlistDecl
AttlistDecl        ::= '<!ATTLIST' S Name <AttDef any> <S maybe> '>'
# event AttDef$ = completed AttDef
AttDef             ::= S Name S AttType S DefaultDecl
# event AttType$ = completed AttType
AttType            ::= StringType | TokenizedType | EnumeratedType
# event StringType$ = completed StringType
StringType         ::= 'CDATA'
# event TokenizedType$ = completed TokenizedType
TokenizedType      ::= 'ID'
                     | 'IDREF'
                     | 'IDREFS'
                     | 'ENTITY'
                     | 'ENTITIES'
                     | 'NMTOKEN'
                     | 'NMTOKENS'
# event EnumeratedType$ = completed EnumeratedType
EnumeratedType     ::= NotationType
                     | Enumeration
# event NotationType$ = completed NotationType
NotationType       ::= 'NOTATION' S '(' <S maybe> Name    <NotationType1 any> <S maybe> ')'
# event Enumeration$ = completed Enumeration
Enumeration        ::=              '(' <S maybe> Nmtoken <Enumeration1 any>  <S maybe> ')'
# event DefaultDecl$ = completed DefaultDecl
DefaultDecl        ::= '#REQUIRED'
                     | '#IMPLIED'
                     |            AttValue
                     | '#FIXED' S AttValue
# event conditionalSect$ = completed conditionalSect
conditionalSect    ::= includeSect | ignoreSect
# event includeSect$ = completed includeSect
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

# event ignoreSect$ = completed ignoreSect
ignoreSect         ::= '<![' <S maybe> 'IGNORE' <S maybe> '[' <ignoreSectContents any> ']]>'
                     | '<![' <S maybe> 'IGNORE' <S maybe> '['                          ']]>'
# event ignoreSectContents$ = completed ignoreSectContents
ignoreSectContents ::= Ignore <ignoreSectContents1 any>
# event Ignore$ = completed Ignore
Ignore             ::= <Ignore Exceptioned>
# event CharRef$ = completed CharRef
CharRef            ::= '&#' <digit many> ';'
                     | '&#x' <hexdigit many> ';'
# event Reference$ = completed Reference
Reference          ::= EntityRef
                     | CharRef
# event EntityRef$ = completed EntityRef
EntityRef          ::= '&' Name ';'
# event PEReference$ = completed PEReference
PEReference        ::= '%' Name ';'
# event EntityDecl$ = completed EntityDecl
EntityDecl         ::= GEDecl | PEDecl
# event GEDecl$ = completed GEDecl
GEDecl             ::= '<!ENTITY' S Name S EntityDef <S maybe> '>'
# event PEDecl$ = completed PEDecl
PEDecl             ::= '<!ENTITY' S '%' S Name S PEDef <S maybe> '>'
# event EntityDef$ = completed EntityDef
EntityDef          ::= EntityValue
                     | ExternalID
                     | ExternalID NDataDecl
# event PEDef$ = completed PEDef
PEDef              ::= EntityValue | ExternalID
# event ExternalID$ = completed ExternalID
ExternalID         ::= 'SYSTEM' S SystemLiteral
                     | 'PUBLIC' S PubidLiteral S SystemLiteral
# event NDataDecl$ = completed NDataDecl
NDataDecl          ::= S 'NDATA' S Name
# event TextDecl$ = completed TextDecl
#
# Note: it is important to split '<?xml' into '<?' 'xml' because of PI whose defintion is: '<?' PITarget
#
TextDecl           ::= '<?' 'xml' <VersionInfo maybe> EncodingDecl <S maybe> '?>'
# event extParsedEnt$ = completed extParsedEnt
extParsedEnt       ::= <TextDecl maybe> content
# event EncodingDecl$ = completed EncodingDecl
EncodingDecl       ::= S 'encoding' Eq '"' EncName '"'                               ## Decl_action => ::copy[4]
                     | S 'encoding' Eq "'" EncName "'"                               ## Decl_action => ::copy[4]
# event EncName$ = completed EncName
EncName            ::= <EncName header> <EncName trailer any>
# event NotationDecl$ = completed NotationDecl
NotationDecl       ::= '<!NOTATION' S Name S ExternalID <S maybe> '>'
                     | '<!NOTATION' S Name S PublicID   <S maybe> '>'
# event PublicID$ = completed PublicID
PublicID           ::= 'PUBLIC' S PubidLiteral

# event Misc_any$ = completed <Misc any>
<Misc any>                ::= Misc*
# event NameChar_any$ = completed <NameChar any>
<NameChar any>            ::= NameChar*
# event EntityValue1$ = completed <EntityValue1>
<EntityValue1>            ::= EntityValueDQInner
                            | PEReference
                            | Reference
# event EntityValue2$ = completed <EntityValue2>
<EntityValue2>            ::= EntityValueSQInner
                            | PEReference
                            | Reference
# event EntityValue1_any$ = completed <EntityValue1 any>
<EntityValue1 any>        ::= <EntityValue1>*
# event EntityValue2_any$ = completed <EntityValue2 any>
<EntityValue2 any>        ::= <EntityValue2>*
# event AttValue1$ = completed <AttValue1>
<AttValue1>               ::= AttValueDQInner
                            | Reference
# event AttValue2$ = completed <AttValue2>
<AttValue2>               ::= AttValueSQInner
                            | Reference
# event AttValue1_any$ = completed <AttValue1 any>
<AttValue1 any>           ::= <AttValue1>*
# event AttValue2_any$ = completed <AttValue2 any>
<AttValue2 any>           ::= <AttValue2>*
# event SystemLiteral1$ = completed <SystemLiteral1>
<SystemLiteral1>          ::= SystemLiteralDQInner
# event SystemLiteral2$ = completed <SystemLiteral2>
<SystemLiteral2>          ::= SystemLiteralSQInner
# event SystemLiteral1_any$ = completed <SystemLiteral1 any>
<SystemLiteral1 any>      ::= <SystemLiteral1>*
# event SystemLiteral2_any$ = completed <SystemLiteral2 any>
<SystemLiteral2 any>      ::= <SystemLiteral2>*
# event PubidChar1_any$ = completed <PubidChar1 any>
<PubidChar1 any>          ::= PubidChar*
# event PubidChar2$ = completed <PubidChar2>
<PubidChar2>              ::= [\x{20}\x{D}\x{A}a-zA-Z0-9\-()+,./:=?;!*#@$_%]  # Same as PubidChar but without '
# event PubidChar2_any$ = completed <PubidChar2 any>
<PubidChar2 any>          ::= <PubidChar2>*
# event XMLDecl_maybe$ = completed <XMLDecl maybe>
<XMLDecl maybe>           ::= XMLDecl
<XMLDecl maybe>           ::=
# event EncodingDecl_maybe$ = completed <EncodingDecl maybe>
<EncodingDecl maybe>      ::= EncodingDecl
<EncodingDecl maybe>      ::=
# event SDDecl_maybe$ = completed <SDDecl maybe>
<SDDecl maybe>            ::= SDDecl
<SDDecl maybe>            ::=
# event S_maybe$ = completed <S maybe>
<S maybe>                 ::= S
<S maybe>                 ::=
# event digit$ = completed <digit>
<digit>                   ::= [0-9]
# event digit_many$ = completed <digit many>
<digit many>              ::= <digit>+
# event hexdigit$ = completed <hexdigit>
<hexdigit>                ::= [0-9a-fA-F]
# event hexdigit_many$ = completed <hexdigit many>
<hexdigit many>           ::= <hexdigit>+
# event intSubset1$ = completed <intSubset1>
<intSubset1>              ::= markupdecl
                            | DeclSep
# event intSubset1_any$ = completed <intSubset1 any>
<intSubset1 any>          ::= <intSubset1>*
# event extSubsetDecl1$ = completed <extSubsetDecl1>
<extSubsetDecl1>          ::= markupdecl
                            | conditionalSect
                            | DeclSep
# event extSubsetDecl1_any$ = completed <extSubsetDecl1 any>
<extSubsetDecl1 any>      ::= <extSubsetDecl1>*
# event yes_or_no$ = completed <yes or no>
<yes or no>               ::= 'yes' | 'no'
# event STag1$ = completed <STag1>
<STag1>                   ::= S Attribute
# event STag1_any$ = completed <STag1 any>
<STag1 any>               ::= <STag1>*
# event CharData_maybe$ = completed <CharData maybe>
<CharData maybe>          ::= CharData
<CharData maybe>          ::=
# event content1$ = completed <content1>
<content1>                ::= element   <CharData maybe>
                            | Reference <CharData maybe>
                            | CDSect    <CharData maybe>
                            | PI        <CharData maybe>
                            | Comment   <CharData maybe>
# event content1_any$ = completed <content1 any>
<content1 any>            ::= <content1>*
# event EmptyElemTag1$ = completed <EmptyElemTag1>
<EmptyElemTag1>           ::= S Attribute
# event EmptyElemTag1_any$ = completed <EmptyElemTag1 any>
<EmptyElemTag1 any>       ::= <EmptyElemTag1>*
# event choice_or_seq$ = completed <choice or seq>
<choice or seq>           ::= choice | seq
# event sequence$ = completed <sequence>
<sequence>                ::= '?' | '*' | '+'
# event sequence_maybe$ = completed <sequence maybe>
<sequence maybe>          ::= <sequence>
<sequence maybe>          ::=
# event Name_or_choice_or_seq$ = completed <Name or choice or seq>
<Name or choice or seq>   ::= Name | choice | seq
# event choice1$ = completed <choice1>
<choice1>                 ::= <S maybe> '|' <S maybe> cp
# event choice1_many$ = completed <choice1 many>
<choice1 many>            ::= <choice1>+
# event seq1$ = completed <seq1>
<seq1>                    ::= <S maybe> ',' <S maybe> cp
# event seq1_any$ = completed <seq1 any>
<seq1 any>                ::= <seq1>*
# event Mixed1$ = completed <Mixed1>
<Mixed1>                  ::= <S maybe> '|' <S maybe> Name
# event Mixed1_any$ = completed <Mixed1 any>
<Mixed1 any>              ::= <Mixed1>*
# event AttDef_any$ = completed <AttDef any>
<AttDef any>              ::= AttDef*
# event NotationType1$ = completed <NotationType1>
<NotationType1>           ::= <S maybe> '|' <S maybe> Name
# event NotationType1_any$ = completed <NotationType1 any>
<NotationType1 any>       ::= <NotationType1>*
# event Enumeration1$ = completed <Enumeration1>
<Enumeration1>            ::= <S maybe> '|' <S maybe> Nmtoken
# event Enumeration1_any$ = completed <Enumeration1 any>
<Enumeration1 any>        ::= <Enumeration1>*
# event ignoreSectContents_any$ = completed <ignoreSectContents any>
<ignoreSectContents any>  ::= ignoreSectContents*
# event ignoreSectContents1$ = completed <ignoreSectContents1>
<ignoreSectContents1>     ::= '<![' ignoreSectContents ']]>' Ignore
# event ignoreSectContents1_any$ = completed <ignoreSectContents1 any>
<ignoreSectContents1 any> ::= <ignoreSectContents1>*
# event VersionInfo_maybe$ = completed <VersionInfo maybe>
<VersionInfo maybe>       ::= VersionInfo
<VersionInfo maybe>       ::=
# event TextDecl_maybe$ = completed <TextDecl maybe>
<TextDecl maybe>          ::= TextDecl
<TextDecl maybe>          ::=
# event EncName_header$ = completed <EncName header>
<EncName header>          ::= [A-Za-z]
# event EncName_trailer$ = completed <EncName trailer>
<EncName trailer>         ::= [A-Za-z0-9._-]
# event EncName_trailer_any$ = completed <EncName trailer any>
<EncName trailer any>     ::= <EncName trailer>*

#############################
# Grammar subtilities
#############################
# event EntityValueDQInner$ = completed <EntityValueDQInner>
<EntityValueDQInner>      ::= [\x{9}\x{A}\x{D}\x{20}-\x{21}\x{23}-\x{24}\x{27}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u
# event EntityValueSQInner$ = completed <EntityValueSQInner>
<EntityValueSQInner>      ::= [\x{9}\x{A}\x{D}\x{20}-\x{24}\x{28}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u
# event AttValueDQInner$ = completed <AttValueDQInner>
<AttValueDQInner>         ::= /[\x{9}\x{A}\x{D}\x{20}-\x{21}\x{23}-\x{25}\x{27}-\x{3b}\x{3d}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]+/u
# event AttValueSQInner$ = completed <AttValueSQInner>
<AttValueSQInner>      ::= /[\x{9}\x{A}\x{D}\x{20}-\x{25}\x{28}-\x{3b}\x{3d}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]+/u
# event SystemLiteralDQInner$ = completed <SystemLiteralDQInner>
SystemLiteralDQInner      ::= /[\x{9}\x{A}\x{D}\x{20}-\x{21}\x{23}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]+/u
# event SystemLiteralSQInner$ = completed <SystemLiteralSQInner>
SystemLiteralSQInner      ::= /[\x{9}\x{A}\x{D}\x{20}-\x{26}\x{28}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]+/u

#############################
# For element start detection
#############################
:lexeme ::= ELEMENT_START pause => before event => ^ELEMENT_START
ELEMENT_START               ~ '<'

#############################
# For element end detection
#############################
:lexeme ::= ELEMENT_END1 pause => before event => ^ELEMENT_END1=off
ELEMENT_END1                ~ '>'

:lexeme ::= ELEMENT_END2 pause => before event => ^ELEMENT_END2=off
ELEMENT_END2                ~ '/>'

#############################
# For element valuation injected in parent recognizer
#############################
ELEMENT_VALUE               ~ [^\s\S]

#########
# Lexemes
#########
# <_NAMESTARTCHAR>           ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
# <_NAMECHAR>                ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
# <_NAMECHAR any>            ~ <_NAMECHAR>*
# <_NAME>                    ~ <_NAMESTARTCHAR> <_NAMECHAR any>

<_NAME>                    ~ /[:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}][:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]*/u
# :lexeme ::= NAME pause => after event => NAME$
<NAME>                     ~ <_NAME>

################
# XML Exceptions
################
#
# -------------------------------------------------------------
# Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
# -------------------------------------------------------------
#
# event Char_minus_sign$ = completed <Char minus sign>
<Char minus sign>       ::= [\x{9}\x{A}\x{D}\x{20}-\x{2C}\x{2E}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u  # '-' is \x{2D}
# event Comment_Interior_Unit$ = completed <Comment Interior Unit>
<Comment Interior Unit> ::=     <Char minus sign>
                          | '-' <Char minus sign>
# event Comment_Interior$ = completed <Comment Interior>
<Comment Interior>      ::= <Comment Interior Unit>*
#
# -----------------------------------------------------------
# PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
# -----------------------------------------------------------
#
# No need for exception, because '?>' is longer than Char
#
# event PI_Exceptioned$ = completed <PI Exceptioned>
<PI Exceptioned>        ::= Char*
#
# ---------------------------------------------------------
# PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
# ---------------------------------------------------------
#
# The following is working, but we want this module to be
# more user-friendly, saying that a PITarget cannot be 'xml':i more explicitly.
# Since we will use events anyway because of SAX support, we add an explicit
#event for PITarget
<_XML>                     ~ [Xx] [Mm] [Ll]
# event PITarget$ = completed PITarget
<PITarget>              ::= <_NAME> - <_XML>

#
# If you like to handle this in user-space, this could be... with an event on PITarget$, then getting lastLexemePause('PITarget'):
#
# <NAMESTARTCHAR>           ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
# <NAMECHAR>                ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
# <NAMECHAR any>            ~ <NAMECHAR>*
# <NAME>                    ~ <NAMESTARTCHAR> <NAMECHAR any>
# :lexeme ::= PITarget pause => after event => PITarget$
# <PITarget>                ~ <NAME>

#
# ---------------------------------------
# CData ::= (Char* - (Char* ']]>' Char*))
# ---------------------------------------
#
# No need for exception, because ']]>' is longer than Char
#
# event CData_Exceptioned$ = completed <CData Exceptioned>
<CData Exceptioned>     ::= Char*
#
# ------------------------------------------------
# Ignore ::= Char+ - (Char+ ('<![' | ']]>') Char+)
# ------------------------------------------------
#
# Note that we made Ignore not nullable.
# No need for exception, because '<![' and ']]>' are longer than Char
#
# event Ignore_Exceptioned$ = completed <Ignore Exceptioned>
<Ignore Exceptioned>    ::= Char+
#
# -------------------------------------------
# CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
# -------------------------------------------
#
# Note that we made CharData not nullable.
# No need for exception, because ']]>' is longer than <CharData Unit>
#
# All text that is not markup constitutes the character data of the document, and since
# a character data cannot contain markup characters (nor CDATA section-close delimiter)
# we raise its priority.
#
<_CHARDATA UNIT>          ~ [\x{9}\x{A}\x{D}\x{20}-\x{25}\x{26}-\x{3b}\x{3d}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u
<_CHARDATA UNIT ANY>      ~ <_CHARDATA UNIT>*
<CHARDATA>                ~ <_CHARDATA UNIT ANY>
#<CHARDATA>                ~ /[\x{9}\x{A}\x{D}\x{20}-\x{25}\x{26}-\x{3b}\x{3d}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]+/u
<CHARDATA EXCEPTION>      ~ /.*\]\]>/u  # Faster with a regexp, because it works on an already matched area: <CHARDATA>, so no need to rematch <_CHARDATA UNIT ANY>

# :lexeme ::= CHARDATA pause => after event => CharData_Exceptioned$
<CharData Exceptioned>  ::= <CHARDATA> - <CHARDATA EXCEPTION>

#event CharData_Unit$ = completed <CharData Unit>
#<CharData Unit>         ::= [^<&]
#event CharData_Exceptioned$ = completed <CharData Exceptioned>
#<CharData Exceptioned>  ::= <CharData Unit>+
#
