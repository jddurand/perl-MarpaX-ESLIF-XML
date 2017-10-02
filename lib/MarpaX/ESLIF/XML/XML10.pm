use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::XML10;
use MarpaX::ESLIF;
use MarpaX::ESLIF::XML::RecognizerInterface;
use MarpaX::ESLIF::XML::ValueInterface;
use Data::Section -setup;

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

my $XMLDecl = ${__PACKAGE__->section_data('XMLDecl')};
my $XML     = ${__PACKAGE__->section_data('XML')};

sub new {
    my ($pkg, %options) = @_;

    my $logger = delete($options{logger});
    my $eslif = delete($options{eslif}) // MarpaX::ESLIF->new($logger);

    return bless {
        grammar_xmldecl => MarpaX::ESLIF::Grammar->new($eslif, $XMLDecl),
        grammar         => MarpaX::ESLIF::Grammar->new($eslif, $XML),
        %options
    }, $pkg
}

sub parse {
    my ($self, $data, $encoding) = @_;

    if (! defined($encoding)) {
        #
        # If encoding is not given by the caller, we want to determine it ourself
        # before the parse is starting. This may fail.
        #
    }
    my $encoding;
    my $can_again = 1;
  again:
    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface->new(%{$self->{options}}, data => $data, encoding => $encoding);
    my $valueInterface      = MarpaX::ESLIF::XML::ValueInterface->new(%{$self->{options}});

    if (1) {
        my $eslifRecognizer = MarpaX::ESLIF::Recognizer->new($self->{grammar}, $recognizerInterface);
        $eslifRecognizer->scan() || die "scan() failed";
        $self->_manage_events($eslifRecognizer, $data, \$encoding);
        if ($can_again && defined($encoding)) {
            # print STDERR "==> Using encoding: " . ($encoding // '<undef>') . "\n";
            $can_again = 0;
            goto again;
        }
        if ($eslifRecognizer->isCanContinue) {
            do {
                $eslifRecognizer->resume || die "resume() failed";
                $self->_manage_events($eslifRecognizer, $data, \$encoding);
                    if ($can_again && defined($encoding)) {
                        $can_again = 0;
                        goto again;
                }
            } while ($eslifRecognizer->isCanContinue)
        }
        #
        # We configured value interface to not accept ambiguity not null parse.
        # So no need to loop on value()
        #
        MarpaX::ESLIF::Value->new($eslifRecognizer, $valueInterface)->value()
    } else {    
        $self->{grammar}->parse($recognizerInterface, $valueInterface);
    }
}

sub _manage_events {
    my ($self, $eslifRecognizer, $data, $encoding_ref) = @_;

    foreach (@{$eslifRecognizer->events()}) {
        my $event = $_->{event};
        next unless $event;  # Can be undef for exhaustion
        my $symbol = $_->{symbol};
        next unless $symbol;  # should never happen
        if ($event eq 'ENCNAME$') {
            ${$encoding_ref} = $eslifRecognizer->lexemeLastPause($symbol);
            print STDERR "... Encoding declared to: ${$encoding_ref}\n";
            next;
        }
        if ($event eq 'checkEncoding[]') {
            if (! defined(${$encoding_ref})) {
                ${$encoding_ref} = 'UTF-8';
                print STDERR "... Encoding forced to: ${$encoding_ref}\n";
            }
            next;
        }
    }
}

1;

__DATA__
__[ XMLDecl ]__
XMLDecl              ::= '<?xml' VersionInfo <EncodingDecl maybe> <SDDecl maybe> <S maybe> '?>'
VersionInfo          ::= S 'version' Eq "'" VersionNum "'" | S 'version' Eq '"' VersionNum '"'
EncodingDecl         ::= S 'encoding' Eq '"' EncName '"'
                       | S 'encoding' Eq "'" EncName "'"
S                    ::= [\x{20}\x{9}\x{D}\x{A}]:u+
<EncodingDecl maybe> ::= EncodingDecl
<EncodingDecl maybe> ::=
Eq                   ::= <S maybe> '=' <S maybe>
SDDecl               ::= S 'standalone' Eq "'" <yes or no> "'" | S 'standalone' Eq '"' <yes or no> '"'
<SDDecl maybe>       ::= SDDecl
<SDDecl maybe>       ::=
<S maybe>            ::= S
<S maybe>            ::=
VersionNum           ::= '1.' <Digit many>
EncName              ::= ENCNAME
<Digit many>         ::= DIGIT+
<yes or no>          ::= 'yes' | 'no'

:lexeme ::= ENCNAME pause => after event => ENCNAME$
_ENCNAME_HEADER        ~ [A-Za-z]
_ENCNAME_TRAILER       ~ [A-Za-z0-9._-]*
_ENCNAME               ~ _ENCNAME_HEADER _ENCNAME_TRAILER
ENCNAME                ~ _ENCNAME

_DIGIT                 ~ [0-9]
DIGIT                  ~ _DIGIT
__[ XML ]__
#
# From https://www.w3.org/TR/REC-xml (5th edition)
#
# Take care, original has several ambiguities:
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0000 (applied)
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001 (applied)
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002 (applied)
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0003 (reply to the above mails)
# - http://lists.xml.org/archives/xml-dev/200005/msg00549.html      (not applied)
#
# All literals are expressed via lexemes. This allow in particular the user of exceptions
# in ESLIF, that required both sides of an exception to be lexemes.
#
# By convention all helpers are enclosed (<>), all lexemes are in CAPITAL LETTERS, internal symbols start with x underscores (_).

document      ::= prolog element <Misc any>
Char          ::= CHAR
S             ::= S1+
Name          ::= NAME
Names         ::= Name+    separator => [\x{20}]
Nmtoken       ::= NMTOKEN
Nmtokens      ::= Nmtoken+ separator => [\x{20}]

EntityValue   ::= '"' <EntityValue Dquote any>   '"' | "'" <EntityValue Squote any>     "'"
AttValue      ::= '"' <AttValue Dquote any>      '"' | "'" <AttValue Squote any>        "'"
SystemLiteral ::= '"' <SystemLiteral Dquote any> '"' | "'" <SystemLiteral Squote any>   "'" 
PubidLiteral  ::= '"' <PubidChar any>            '"' | "'" <PubidChar Without Quote any> "'" # No need of exception for PubidChar without "'"
PubidChar     ::= PUBIDCHAR

CharData      ::= CHARDATA_UNIT+                              # https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0000)

Comment       ::= '<!--' <Comment Unit any> '-->'

PI            ::= '<?' PITarget <PI Interior maybe> '?>'
PITarget      ::= PITARGET_NAME - XML_CASE_INSENSITIVE

CDSect        ::= CDStart CData CDEnd
CDStart       ::= '<![CDATA['
CData         ::= CHARDATA_UNIT*
CDEnd         ::= ']]>'

prolog        ::= <XMLDecl maybe> <Misc any>
                | <XMLDecl maybe> <Misc any> doctypedecl <Misc any>
XMLDecl       ::= '<?xml' VersionInfo <EncodingDecl maybe> <SDDecl maybe> <S maybe> '?>'
VersionInfo   ::= S 'version' Eq "'" VersionNum "'" | S 'version' Eq '"' VersionNum '"'
Eq            ::= <S maybe> '=' <S maybe>
VersionNum    ::= '1.' <Digit many>
Misc          ::= Comment | PI | S1                           # https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002

doctypedecl   ::= '<!DOCTYPE' S Name <S ExternalID maybe> <S maybe>                             '>'
                | '<!DOCTYPE' S Name <S ExternalID maybe> <S maybe> '[' intSubset ']' <S maybe> '>'
DeclSep       ::= PEReference | S1                            # https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001
intSubset     ::= <intSubset unit any>
markupdecl    ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment

extSubset     ::= <TextDecl maybe> extSubsetDecl
extSubsetDecl ::= <extSubsetDecl unit any>

SDDecl        ::= S 'standalone' Eq "'" <yes or no> "'" | S 'standalone' Eq '"' <yes or no> '"'

element       ::= EmptyElemTag | STag content ETag 

STag          ::= '<' Name <S Attribute any> <S maybe> '>'
Attribute     ::= Name Eq AttValue 

ETag          ::= '</' Name <S maybe> '>'

content       ::= <CharData maybe> <content trailer unit any>

EmptyElemTag  ::= '<' Name <S Attribute any> <S maybe> '/>'

elementdecl   ::= '<!ELEMENT' S Name S contentspec <S maybe> '>'
contentspec   ::= 'EMPTY' | 'ANY' | Mixed | children 

children      ::= <choice or seq> <element content quantifier maybe>
cp            ::= <Name or choice or seq> <element content quantifier maybe>
choice        ::= '(' <S maybe> cp <choice interior many> <S maybe> ')'
seq           ::= '(' <S maybe> cp <seq interior any> <S maybe> ')'

Mixed         ::= '(' <S maybe> '#PCDATA' <Mixed interior any> <S maybe> ')*'
                | '(' <S maybe> '#PCDATA'                      <S maybe> ')'

AttlistDecl   ::= '<!ATTLIST' S Name <AttDef any> <S maybe> '>'
AttDef        ::= S Name S AttType S DefaultDecl 

AttType       ::= StringType | TokenizedType | EnumeratedType
StringType    ::= 'CDATA'
TokenizedType ::= 'ID'
                | 'IDREF'
                | 'IDREFS'
                | 'ENTITY'
                | 'ENTITIES'
                | 'NMTOKEN'
                | 'NMTOKENS'

EnumeratedType ::= NotationType | Enumeration
NotationType   ::= 'NOTATION' S '(' <S maybe> Name    <NotationType unit any> <S maybe> ')'
Enumeration    ::=              '(' <S maybe> Nmtoken <Enumeration unit any>  <S maybe> ')'

DefaultDecl    ::= '#REQUIRED'
                 | '#IMPLIED'
                 |            AttValue
                 | '#FIXED' S AttValue

conditionalSect    ::= includeSect | ignoreSect
includeSect        ::= '<![' <S maybe> 'INCLUDE' <S maybe> '[' extSubsetDecl            ']]>'
ignoreSect         ::= '<![' <S maybe> 'IGNORE'  <S maybe> '[' <ignoreSectContents any> ']]>'
ignoreSectContents ::= Ignore <ignoreSectContents unit any>
Ignore             ::= IGNORE_UNIT*

CharRef        ::= '&#' <Digit many> ';' | '&#x' <Hexdigit many> ';'

Reference      ::= EntityRef | CharRef
EntityRef      ::= '&' Name ';'
PEReference    ::= '%' Name ';'

EntityDecl     ::= GEDecl | PEDecl
GEDecl         ::= '<!ENTITY' S Name S EntityDef <S maybe> '>'
PEDecl         ::= '<!ENTITY' S '%' S Name S PEDef <S maybe> '>'
EntityDef      ::= EntityValue
                 | ExternalID
                 | ExternalID NDataDecl
PEDef          ::= EntityValue | ExternalID

ExternalID     ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
NDataDecl      ::= S 'NDATA' S Name

TextDecl       ::= '<?xml'             EncodingDecl <S maybe> '?>'
                 | '<?xml' VersionInfo EncodingDecl <S maybe> '?>'

extParsedEnt   ::=          <check encoding> content
                 | TextDecl <check encoding> content

EncodingDecl   ::= S 'encoding' Eq '"' EncName '"'
                 | S 'encoding' Eq "'" EncName "'"
EncName        ::= ENCNAME

NotationDecl   ::= '<!NOTATION' S Name S ExternalID <S maybe> '>'
                 | '<!NOTATION' S Name S PublicID   <S maybe> '>'
PublicID       ::= 'PUBLIC' S PubidLiteral

# #########################################################
# Not used at grammar level 0
# #########################################################
# NameStartChar ::= NAMESTARTCHAR
# NameChar      ::= NAMECHAR

# #########################################################
# Helpers
# #########################################################
<Misc any>                    ::= Misc*

<EntityValue Dquote>          ::= [^%&"] | PEReference | Reference
<EntityValue Dquote any>      ::= <EntityValue Dquote>*

<EntityValue Squote>          ::= [^%&'] | PEReference | Reference
<EntityValue Squote any>      ::= <EntityValue Squote>*

<AttValue Dquote>             ::= [^<&"] | Reference
<AttValue Dquote any>         ::= <AttValue Dquote>*

<AttValue Squote>             ::= [^<&'] | Reference
<AttValue Squote any>         ::= <AttValue Squote>*

<SystemLiteral Dquote>        ::= [^"]
<SystemLiteral Dquote any>    ::= <SystemLiteral Dquote>*

<SystemLiteral Squote>        ::= [^']
<SystemLiteral Squote any>    ::= <SystemLiteral Squote>*

<PubidChar any>               ::= PubidChar*
<PubidChar Without Quote>     ::= PUBIDCHAR_WITHOUT_QUOTE
<PubidChar Without Quote any> ::= <PubidChar Without Quote>*

<Comment Unit any>            ::= COMMENT_UNIT*
<PI Unit any>                 ::= PI_UNIT*
<PI Interior maybe>           ::= S <PI Unit any>
<PI Interior maybe>           ::=

<S maybe>                     ::= S
<S maybe>                     ::=
<XMLDecl maybe>               ::= XMLDecl
<XMLDecl maybe>               ::=
<EncodingDecl maybe>          ::= EncodingDecl
<EncodingDecl maybe>          ::=
<SDDecl maybe>                ::= SDDecl
<SDDecl maybe>                ::=
<Digit many>                  ::= DIGIT+
<Hexdigit many>               ::= HEXDIGIT+

<intSubset unit>              ::= markupdecl | DeclSep
<intSubset unit any>          ::= <intSubset unit>*
<S ExternalID>                ::= S ExternalID
<S ExternalID maybe>          ::= <S ExternalID>
<S ExternalID maybe>          ::=

<TextDecl maybe>              ::= TextDecl
<TextDecl maybe>              ::=
<extSubsetDecl unit>          ::= markupdecl | conditionalSect | DeclSep
<extSubsetDecl unit any>      ::= <extSubsetDecl unit>*

<yes or no>                   ::= 'yes' | 'no'

<S Attribute>                 ::= S Attribute
<S Attribute any>             ::= <S Attribute>*

<CharData maybe>              ::= CharData
<CharData maybe>              ::=

<element or Reference or CDSect or PI or Comment> ::= element | Reference | CDSect | PI | Comment
<content trailer unit>        ::= <element or Reference or CDSect or PI or Comment> <CharData maybe>
<content trailer unit any>    ::= <content trailer unit>*

<choice or seq>                    ::= choice | seq
<choice interior>                  ::= <S maybe> '|' <S maybe> cp
<choice interior many>             ::= <choice interior>+
<seq interior>                     ::= <S maybe> ',' <S maybe> cp
<seq interior any>                 ::= <seq interior>*
<Name or choice or seq>            ::= Name | choice | seq
<element content quantifier>       ::= '?' | '*' | '+'
<element content quantifier maybe> ::= <element content quantifier>
<element content quantifier maybe> ::=

<Mixed interior>                   ::= <S maybe> '|' <S maybe> Name
<Mixed interior any>               ::= <Mixed interior>*

<AttDef any>                  ::= AttDef*

<NotationType unit>           ::= <S maybe> '|' <S maybe> Name
<NotationType unit any>       ::= <NotationType unit>*
<Enumeration unit>            ::= <S maybe> '|' <S maybe> Nmtoken
<Enumeration unit any>        ::= <Enumeration unit>*

# Note:
# ignoreSectContents is a nullable and Marpa does not like at all when it is on the right side of a sequence.
# The following rule:
# <ignoreSectContents any> ::= ignoreSectContents*
# will raise:
# MARPA_ERR_COUNTED_NULLABLE: Nullable symbol on RHS of a sequence rule
# So we revisit ignoreSectContents to an <ignoreSectContents not nullable> and do the impact
#
# <ignoreSectContents any> ::= ignoreSectContents*
# becomes:
# <ignoreSectContents any> ::= <ignoreSectContents not nullable>*   # No impact if this is ignoreSectContents or the revisited version: this is a '*' sequence
#
# <ignoreSectContents unit> ::= '<![' ignoreSectContents ']]>' Ignore
# becomes:
# <ignoreSectContents unit> ::= '<![' <ignoreSectContents not nullable> ']]>' Ignore   # ignoreSectContents is not nullable anymore
#                             | '<!['                                   ']]>' Ignore   # so we introduce the absence of ignoreSectContents
#
# The non-nullable version of ignoreSectContents is:
#
# <ignoreSectContents not nullable> ::= <Ignore not nullable> <ignoreSectContents unit any>
#                                     |                       <ignoreSectContents unit many>
# <Ignore not nullable>          ::= <Ignore unit many>
# <Ignore unit many>             ::= <Ignore unit>+
# <ignoreSectContents unit many> ::= <ignoreSectContents unit>+
#
# SHOULD HAVE BEEN:
# <ignoreSectContents any>      ::= ignoreSectContents*
# <ignoreSectContents unit>     ::= '<![' ignoreSectContents ']]>' Ignore
# <ignoreSectContents unit any> ::= <ignoreSectContents unit>*
#
# NOW IS:
<ignoreSectContents any>      ::= <ignoreSectContents not nullable>*
<ignoreSectContents unit>     ::= '<![' <ignoreSectContents not nullable> ']]>' Ignore
                                | '<!['                                   ']]>' Ignore
<ignoreSectContents not nullable> ::= <Ignore not nullable> <ignoreSectContents unit any>
                                    |                       <ignoreSectContents unit many>
<Ignore not nullable>          ::= IGNORE_UNIT+
<ignoreSectContents unit many> ::= <ignoreSectContents unit>+
<ignoreSectContents unit any>  ::= <ignoreSectContents unit>*

<check encoding>               ::=
# #########################################################
# Lexemes
# #########################################################
_CHAR                          ~ [\x{9}\x{A}\x{D}\x{20}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u
_CHAR_WITHOUT_MINUS            ~ [\x{9}\x{A}\x{D}\x{20}-\x{2C}\x{2E}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u # '-' is \x{2D}
_CHAR_WITHOUT_QUESTION_MARK    ~ [\x{9}\x{A}\x{D}\x{20}-\x{3E}\x{40}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u # '?' is \x{3F}
_CHAR_WITHOUT_GREATER_THAN     ~ [\x{9}\x{A}\x{D}\x{20}-\x{3D}\x{3F}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u # '>' is \x{3E}
_CHAR_WITHOUT_RIGHT_BRACKET    ~ [\x{9}\x{A}\x{D}\x{20}-\x{5C}\x{5E}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u # ']' is \x{5D}
_CHAR_WITHOUT_LOWER_THAN       ~ [\x{9}\x{A}\x{D}\x{20}-\x{3B}\x{3D}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u # '<' is \x{3C}
_CHAR_WITHOUT_EXCLAMATION_MARK ~ [\x{9}\x{A}\x{D}\x{20}\x{22}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u        # '!' is \x{21}
_CHAR_WITHOUT_LEFT_BRACKET     ~ [\x{9}\x{A}\x{D}\x{20}-\x{5A}\x{5C}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u # ']' is \x{5B}
_S1                            ~ [\x{20}\x{9}\x{D}\x{A}]:u
_NAMESTARTCHAR                 ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
_NAMECHAR                      ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
<_NAMECHAR any>                ~ _NAMECHAR*
_NMTOKEN                       ~ _NAMECHAR+
_PUBIDCHAR                     ~ [\x{20}\x{D}\x{A}a-zA-Z0-9\-'()+,./:=?;!*#@$_%]
_PUBIDCHAR_WITHOUT_QUOTE       ~ [\x{20}\x{D}\x{A}a-zA-Z0-9\-()+,./:=?;!*#@$_%]
_CHARDATA_UNIT                 ~ ']]' _CHAR_WITHOUT_GREATER_THAN
                               | ']'  _CHAR_WITHOUT_RIGHT_BRACKET
                               |      _CHAR_WITHOUT_RIGHT_BRACKET
_COMMENT_UNIT                  ~ '-' _CHAR_WITHOUT_MINUS
                               |     _CHAR_WITHOUT_MINUS
_PI_UNIT                       ~ '?' _CHAR_WITHOUT_GREATER_THAN
                               |     _CHAR_WITHOUT_QUESTION_MARK
_XML_CASE_INSENSITIVE          ~ 'xml':i
_DIGIT                         ~ [0-9]
_HEXDIGIT                      ~ [0-9a-fA-F]
_IGNORE_UNIT                   ~ '<!' _CHAR_WITHOUT_LEFT_BRACKET
                               | '<'  _CHAR_WITHOUT_EXCLAMATION_MARK
                               |      _CHAR_WITHOUT_LOWER_THAN
                               | _CHARDATA_UNIT
_ENCNAME_HEADER                ~ [A-Za-z]
_ENCNAME_TRAILER               ~ [A-Za-z0-9._-]*
_ENCNAME                       ~ _ENCNAME_HEADER _ENCNAME_TRAILER
_NAME                          ~ _NAMESTARTCHAR <_NAMECHAR any>

CHAR                           ~ _CHAR
S1                             ~ _S1
NAME                           ~ _NAME
PITARGET_NAME                  ~ _NAME
NMTOKEN                        ~ _NMTOKEN
PUBIDCHAR                      ~ _PUBIDCHAR
PUBIDCHAR_WITHOUT_QUOTE        ~ _PUBIDCHAR_WITHOUT_QUOTE
CHARDATA_UNIT                  ~ _CHARDATA_UNIT
COMMENT_UNIT                   ~ _COMMENT_UNIT
PI_UNIT                        ~ _PI_UNIT
XML_CASE_INSENSITIVE           ~ _XML_CASE_INSENSITIVE
DIGIT                          ~ _DIGIT
HEXDIGIT                       ~ _HEXDIGIT
IGNORE_UNIT                    ~ _IGNORE_UNIT
ENCNAME                        ~ _ENCNAME
