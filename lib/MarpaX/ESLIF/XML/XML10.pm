use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::XML10;
use Carp qw/croak/;
use Class::Tiny qw/reader/;
use Log::Any qw/$log/;
use Role::Tiny::With;

with 'MarpaX::ESLIF::XML::Base';

# ABSTRACT: XML 1.0 parser

# VERSION

# AUTHORITY

=head1 DESCRIPTION

XML 1.0 parser.

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML10;

    my $xml10   = MarpaX::ESLIF::XML::XML10->new();
    my $input   = '<?xml></xml>';
    my $xmlhash = $eslifxml10->parse($input);

=cut

my $DATA = do { local $/; <DATA> };

# ----------------------------------------------------------------------------
# Main XML 1.0 grammar
# ----------------------------------------------------------------------------
my $XML_SOURCE  = $DATA;

# ----------------------------------------------------------------------------
# Grammar at document
# ----------------------------------------------------------------------------
my $DOCUMENT_SOURCE = ":start ::= document\n$XML_SOURCE";
my $DOCUMENT_GRAMMAR;
sub document_grammar {
    return $DOCUMENT_GRAMMAR //= MarpaX::ESLIF::Grammar->new(__PACKAGE__->eslif, $DOCUMENT_SOURCE)
}

# ----------------------------------------------------------------------------
# Grammar at XMLDecl
# ----------------------------------------------------------------------------
my $DECL_SOURCE = ":start ::= XMLDecl\n$XML_SOURCE";
$DECL_SOURCE      =~ s/## Decl_//g; # Enable specific decl actions
my $DECL_GRAMMAR;

sub decl_grammar {
    return $DECL_GRAMMAR //= MarpaX::ESLIF::Grammar->new(__PACKAGE__->eslif, $DECL_SOURCE)
}

# ----------------------------------------------------------------------------
# Grammar at element
# ----------------------------------------------------------------------------
my $ELEMENT_SOURCE = ":start ::= element\n$XML_SOURCE";
my $ELEMENT_GRAMMAR;
sub element_grammar {
    return $ELEMENT_GRAMMAR //= MarpaX::ESLIF::Grammar->new(__PACKAGE__->eslif, $ELEMENT_SOURCE)
}

# ----------------------------------------------------------------------------
# Grammr at extParsedEnt
# ----------------------------------------------------------------------------
my $EXTPARSEDENT_SOURCE = ":start ::= extParsedEnt\n$XML_SOURCE";
my $EXTPARSEDENT_GRAMMAR;

sub extParsedEnt_grammar {
    return $EXTPARSEDENT_GRAMMAR //= MarpaX::ESLIF::Grammar->new(__PACKAGE__->eslif, $EXTPARSEDENT_SOURCE);
}

1;

__DATA__
#
# From https://www.w3.org/TR/REC-xml (5th edition)
#
# Take care, "official" grammar has several ambiguities:
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0000
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002
#
# Note:
# Concerning XML Exceptions there are three categories:
# - Comment interior: We create a character class without for Char minus the '-' character
# - PITarget        : Native BNF exception is doing it
# - Others          : They are ALL in the form: <character> - ( <character>* <exception longer than one character> <character>* )
#                     where <exception longer than one character> is always an expected terminal preceeding and/or succeeding <character>* !
#                     So this mean there is NO needed to write exception...: the grammar will natively stop <character>* parsing as soon
#                     as it sees <exception longer than one character> in stream, because it is always working in the LATM (Longest Acceptable
#                     Token Match) mode

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
