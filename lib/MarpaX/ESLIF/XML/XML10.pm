use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::XML10;
use MarpaX::ESLIF;
use MarpaX::ESLIF::XML::RecognizerInterface;
use MarpaX::ESLIF::XML::ValueInterface;

# ABSTRACT: XML 1.1 suite using MarpaX::ESLIF

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module is the XML 1.1 implementation L<MarpaX::ESLIF::XML>.

=head1 SYNOPSIS

    use MarpaX::ESLIF::XML10;

    my $eslifxml10 = MarpaX::ESLIF::XML::XML10->new();
    my $input      = '<?xml></xml>';
    my $xmlhash    = $eslifxml10->parse($input);

=cut

my $_BNF = do { local $/; <DATA> };

sub new {
  my ($pkg, %options) = @_;

  bless {
         grammar => MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($options{logger}), $_BNF),
         %options
        }, $pkg
}

sub parse {
    my ($self, $data) = @_;

    my $recognizerInterface = MarpaX::ESLIF::XML::RecognizerInterface->new(%{$self->{options}}, data => $data);
    my $valueInterface      = MarpaX::ESLIF::XML::ValueInterface->new(%{$self->{options}});

    if (1) {
        my $eslifRecognizer = MarpaX::ESLIF::Recognizer->new($self->{grammar}, $recognizerInterface);
        $eslifRecognizer->scan() || die "scan() failed";
        $self->_manage_events($eslifRecognizer, $data);
        if ($eslifRecognizer->isCanContinue) {
            do {
                $eslifRecognizer->resume || die "resume() failed";
                $self->_manage_events($eslifRecognizer, $data)
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
  my ($self, $eslifRecognizer, $data) = @_;

  foreach (@{$eslifRecognizer->events()}) {
    my $event = $_->{event};
    next unless $event;  # Can be undef for exhaustion
    if ($event eq 'CData$') {
        my ($offset, $length) = $eslifRecognizer->lastCompletedLocation('CData');
        my $CData = substr($data, $offset, $length);
        print STDERR "==> CDATA offset $offset length $length: $CData\n";
    }
  }
}

1;

__DATA__

#
# Document
# --------
document      ::= prolog element Misc_any
#
# Character Range
# ---------------
Char          ::= CHAR
#
# White Space
# -----------
S             ::= /[\x{20}\x{9}\x{D}\x{A}]+/
#
# Names and Tokens
# ----------------
NameStartChar ::= NAMESTARTCHAR
NameChar      ::= NAMECHAR
Name          ::= NAME
Names         ::= Name+ separator => [\x{20}]
Nmtoken       ::= NameChar+
Nmtokens      ::= Nmtoken+ separator => [\x{20}]
#
# Literals
# --------
EntityValue   ::= '"' EntityValueInterior01_any '"'
                | "'" EntityValueInterior02_any "'"
AttValue      ::= '"' AttValueInterior01_any '"'
                | "'" AttValueInterior02_any "'"
SystemLiteral ::= '"' SystemLiteralInterior01_any '"'
                | "'" SystemLiteralInterior02_any "'"
PubidLiteral  ::= '"' PubidLiteralInterior01_any '"'
                | "'" PubidLiteralInterior02_any "'"
PubidChar01   ::= [\x{20}\x{D}\x{A}a-zA-Z0-9\-'()+,./:=?;!*#@$_%] # PubidChar01 = <Original PubidChar>
PubidChar02   ::= [\x{20}\x{D}\x{A}a-zA-Z0-9\-()+,./:=?;!*#@$_%]  # PubidChar02 = <Original PubidChar> - "'" // no need for an exception when the case is easy)
#
# Character Data
# --------------
CharData      ::= CHARSDATA - CHARSDATA_EXCEPTION   # Take care, rewriting it as an exception removed its nullable aspect
                                                    # BUT this is an error in the original spec: CharData should NOT be
                                                    # nullable, c.f. http://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0004.html
#
# Comment
# -------
Comment       ::= '<!--' CommentCharInterior_any '-->'
#
# Processing Instructions
# -----------------------
PI            ::= '<?' PITarget  S PICharsInterior '?>'
                | '<?' PITarget                    '?>'
PITarget      ::= PINAME - PINAME_EXCEPTION                   # Note that a PITarget cannot be nullable
#
# CDATA Sections
# --------------
CDSect        ::= CDStart CData CDEnd
CDStart       ::= '<![CDATA['
# A CDATA character sequence is:
#          ]] followed by CHAR but >, or
#          ]  followed by CHAR but ], or
# CHAR but ]
event CData$  = completed  CData /* For debug */
CDataInterior ::= ']]' CHAR_MINUS_GT
                | ']'  CHAR_MINUS_RBRACKET
                |      CHAR_MINUS_RBRACKET
CData         ::= CDataInterior*

CDEnd         ::= ']]>'
#
# Prolog
# ------
prolog        ::= XMLDecl_maybe Misc_any
                | XMLDecl_maybe Misc_any doctypedecl Misc_any
XMLDecl       ::= '<?xml' VersionInfo EncodingDecl_maybe SDDecl_maybe S_maybe '?>'
VersionInfo   ::= S 'version' Eq "'" VersionNum "'"
                | S 'version' Eq '"' VersionNum '"'
Eq            ::= S_maybe '=' S_maybe
VersionNum    ::= '1.' /[0-9]+/
Misc          ::= Comment
                | PI
                | S
#
# Document Type Definition
# ------------------------
doctypedecl   ::= '<!DOCTYPE' S Name S_ExternalID_maybe S_maybe intSubset_maybe '>'
DeclSep       ::= PEReference
                | S
intSubset     ::= intSubsetInterior*
markupdecl    ::= elementdecl
                | AttlistDecl
                | EntityDecl
                | NotationDecl
                | PI
                | Comment
#
# External Subset
# ---------------
extSubset     ::=          extSubsetDecl
                | TextDecl extSubsetDecl
extSubsetDecl ::= extSubsetDeclInterior_any
#
# Standalone Document Declaration
# -------------------------------
SDDecl        ::= S 'standalone' Eq "'" 'yes' "'"
                | S 'standalone' Eq "'" 'no' "'"
                | S 'standalone' Eq '"' 'yes' '"'
                | S 'standalone' Eq '"' 'no' '"'
#
# Element
# -------
element       ::= EmptyElemTag
                | STag content ETag
#
# Start-tag
# ---------
STag          ::= '<' Name STagInterior_any S_maybe '>'
Attribute     ::= Name Eq AttValue
#
# End-tag
# -------
ETag          ::= '</' Name S_maybe '>'
#
# Content of Elements
# -------------------
content       ::= CharData_maybe contentInterior_any
#
# Tags for Empty Elements
# -----------------------
EmptyElemTag  ::= '<' Name EmptyElemTagInterior_any S_maybe '/>'
#
# Element Type Declaration
# ------------------------
elementdecl   ::= '<!ELEMENT' S Name S contentspec S_maybe '>'
contentspec   ::= 'EMPTY'
                | 'ANY'
                | Mixed
                | children
#
# Element-content Models
# ----------------------
children      ::= choice ElementContentModelQuantifier_maybe
                | seq ElementContentModelQuantifier_maybe
cp            ::= Name ElementContentModelQuantifier_maybe
                | choice ElementContentModelQuantifier_maybe
                | seq ElementContentModelQuantifier_maybe
choice        ::= '(' S_maybe cp choiceInterior_many S_maybe ')'
seq           ::= '(' S_maybe cp seqInterior_any S_maybe ')'
#
# Mixed-content Declaration
# -------------------------
Mixed         ::= '(' S_maybe '#PCDATA' MixedInterior_any S_maybe ')*'
                | '(' S_maybe '#PCDATA'                   S_maybe ')'
#
# Attribute-list Declaration
# --------------------------
AttlistDecl   ::= '<!ATTLIST' S Name AttDef_any S_maybe '>'
AttDef        ::= S Name S AttType S DefaultDecl
#
# Attribute Types
# ---------------
AttType       ::= StringType
                | TokenizedType
                | EnumeratedType
StringType    ::= 'CDATA'
TokenizedType ::= 'ID'
                | 'IDREF'
                | 'IDREFS'
                | 'ENTITY'
                | 'ENTITIES'
                | 'NMTOKEN'
                | 'NMTOKENS'
#
# Enumerated Attribute Types
# --------------------------
EnumeratedType::= NotationType
                | Enumeration
NotationType  ::= 'NOTATION' S '(' S_maybe Name NotationTypeInterior_any S_maybe ')'
Enumeration   ::= '(' S_maybe Nmtoken EnumerationInterior_any S_maybe ')'
#
# Attribute Defaults
# ------------------
DefaultDecl  ::= '#REQUIRED'
               | '#IMPLIED'
               | AttValue
               | '#FIXED' S AttValue
#
# Conditional Section
# -------------------
#
# Originally, ignore section is:
#
# [63]          ignoreSect         ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
# [64]          ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
#
# Ignore is nullable, ignoreSectContents is nullable, and ignoreSect uses ignoreSectContents*.
# But Marpa does not like at all nullables that are in the RHS of a sequence. So we make ignoreSectContents not nullable, and insert a new rule, i.e.:
#
# [63]          ignoreSect         ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
# [63bis]       ignoreSect         ::= '<![' S? 'IGNORE' S? '['                     ']]>'
# [64]          ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)+
# 
conditionalSect ::= includeSect
                  | ignoreSect
includeSect     ::= '<![' S_maybe 'INCLUDE' S_maybe '[' extSubsetDecl ']]>'
ignoreSect      ::= '<![' S_maybe 'IGNORE' S_maybe '[' ignoreSectContents_any ']]>'
                  | '<![' S_maybe 'IGNORE' S_maybe '['                        ']]>'
ignoreSectContents ::= Ignore ignoreSectContentsInterior_many
Ignore          ::= IGNORECHARS - IGNORECHARS_EXCEPTION
Ignore          ::=


#
# Character Reference
# -------------------
CharRef         ::= '&#' /[0-9]+/ ';'
                  | '&#x' /[0-9a-fA-F]+/ ';'
#
# Entity Reference
# ----------------
Reference       ::= EntityRef
                  | CharRef
EntityRef       ::= '&' Name ';'
PEReference     ::= '%' Name ';'

#
# Entity Declaration
# ------------------
EntityDecl      ::= GEDecl
                  | PEDecl
GEDecl          ::= '<!ENTITY' S Name S EntityDef S_maybe '>'
PEDecl          ::= '<!ENTITY' S '%' S Name S PEDef S_maybe '>'
EntityDef       ::= EntityValue
                  | ExternalID
                  | ExternalID NDataDecl
PEDef           ::= EntityValue
                  | ExternalID 

#
# External Entity Declaration
# ---------------------------
ExternalID      ::= 'SYSTEM' S SystemLiteral
                  | 'PUBLIC' S PubidLiteral S SystemLiteral
NDataDecl       ::= S 'NDATA' S Name 

#
# Text Declaration
# ----------------
TextDecl        ::= '<?xml'             EncodingDecl S_maybe '?>'
                  | '<?xml' VersionInfo EncodingDecl S_maybe '?>'
#
# Well-Formed External Parsed Entity
# ----------------------------------
extParsedEnt    ::= TextDecl content 
                  |          content 

#
# Encoding Declaration
# --------------------
EncodingDecl    ::= S 'encoding' Eq '"' EncName '"'
                  | S 'encoding' Eq "'" EncName "'"
EncName         ::= /[A-Za-z][A-Za-z0-9._-]*/

#
# Notation Declarations
# ---------------------
NotationDecl    ::= '<!NOTATION' S Name S ExternalID S_maybe '>'
                  | '<!NOTATION' S Name S PublicID   S_maybe '>'
PublicID        ::= 'PUBLIC' S PubidLiteral 
#
# Grammar helpers following ESLIF spec
#
Misc_any                    ::= Misc*
NameChar_any                ::= NameChar*
EntityValueInterior01       ::= [^%&"]
                              | PEReference
                              | Reference
EntityValueInterior01_any   ::= EntityValueInterior01*
EntityValueInterior02       ::= [^%&']
                              | PEReference
                              | Reference
EntityValueInterior02_any   ::= EntityValueInterior02*
AttValueInterior01          ::= [^<&"]
                              | Reference
AttValueInterior01_any      ::= AttValueInterior01*
AttValueInterior02          ::= [^<&']
                              | Reference
AttValueInterior02_any      ::= AttValueInterior02*
SystemLiteralInterior01     ::= [^"]
SystemLiteralInterior01_any ::= SystemLiteralInterior01*
SystemLiteralInterior02     ::= [^']
SystemLiteralInterior02_any ::= SystemLiteralInterior02*
PubidLiteralInterior01      ::= PubidChar01
PubidLiteralInterior01_any  ::= PubidLiteralInterior01*
PubidLiteralInterior02      ::= PubidChar02
PubidLiteralInterior02_any  ::= PubidLiteralInterior02*
CommentChar                 ::= [\x{9}\x{A}\x{D}\x{20}-\x{2c}\x{2e}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u # CommentChar ::= Char - '-' // ^'-' is 0x2d
CommentCharInterior         ::= CommentChar
                              | '-' CommentChar
CommentCharInterior_any     ::= CommentCharInterior*
PICharsInterior             ::= PICHAR*
S_maybe                     ::= S
S_maybe                     ::=
XMLDecl_maybe               ::= XMLDecl
XMLDecl_maybe               ::=
EncodingDecl_maybe          ::= EncodingDecl
EncodingDecl_maybe          ::=
SDDecl_maybe                ::= SDDecl
SDDecl_maybe                ::=
intSubsetInterior           ::= markupdecl
                              | DeclSep
S_ExternalID                ::= S ExternalID
S_ExternalID_maybe          ::= S_ExternalID
S_ExternalID_maybe          ::=
intSubset_maybe             ::= '[' intSubset ']' S_maybe
intSubset_maybe             ::=
extSubsetDeclInterior       ::= markupdecl
                              | conditionalSect
                              | DeclSep
extSubsetDeclInterior_any   ::= extSubsetDeclInterior*
STagInterior                ::= S Attribute
STagInterior_any            ::= STagInterior*
CharData_maybe              ::= CharData
CharData_maybe              ::=
contentInterior             ::= element CharData_maybe
                              | Reference CharData_maybe
                              | CDSect CharData_maybe
                              | PI CharData_maybe
                              | Comment CharData_maybe
contentInterior_any         ::= contentInterior*
EmptyElemTagInterior        ::= S Attribute
EmptyElemTagInterior_any    ::= EmptyElemTagInterior*
ElementContentModelQuantifier ::= '?'
                              | '*'
                              | '+'
ElementContentModelQuantifier_maybe ::= ElementContentModelQuantifier
ElementContentModelQuantifier_maybe ::=
choiceInterior              ::= S_maybe '|' S_maybe cp
choiceInterior_many         ::= choiceInterior+
seqInterior                 ::= S_maybe ',' S_maybe cp
seqInterior_any             ::= seqInterior*
MixedInterior               ::= S_maybe '|' S_maybe Name
MixedInterior_any           ::= MixedInterior*
AttDef_any                  ::= AttDef
NotationTypeInterior        ::= S_maybe '|' S_maybe Name
NotationTypeInterior_any    ::= NotationTypeInterior*
EnumerationInterior         ::= S_maybe '|' S_maybe Nmtoken
EnumerationInterior_any     ::= EnumerationInterior*
ignoreSectContentsInterior  ::= '<![' ignoreSectContents ']]>' Ignore
ignoreSectContentsInterior_many  ::= ignoreSectContentsInterior+
ignoreSectContents_any     ::= ignoreSectContents*
#
# Some lexemes for convenience
# -----------------------------
#
# CharData has a syntactic exception: both ends must be unique lexemes
#
CHARSDATA                     ~ _CHARSDATA
CHARSDATA_EXCEPTION           ~ _CHARSDATA ']]>' _CHARSDATA
_CHARSDATA                    ~ [^<&]*
#
# CHAR is shared as an exception with PICharsInterior CDATACharsInterior
#
CHAR                          ~ _CHAR
PICHAR                        ~ /(?:\?[\x{9}\x{A}\x{D}\x{20}-\x{3D}\x{3F}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]|[\x{9}\x{A}\x{D}\x{20}-\x{3E}\x{40}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}])/u
_CHAR                         ~ [\x{9}\x{A}\x{D}\x{20}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u /* any Unicode character, excluding the surrogate blocks, FFFE, and FFFF. */
_CHAR_any                     ~ _CHAR*
#
# NAME is shared as an exception PITarget
#
NAMESTARTCHAR                 ~ _NAMESTARTCHAR
_NAMESTARTCHAR                ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
NAMECHAR                      ~ _NAMECHAR
_NAMECHAR_any                 ~ _NAMECHAR*
_NAMECHAR                     ~ _NAMESTARTCHAR
                              | [-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
NAME                          ~ _NAME
_NAME                         ~ _NAMESTARTCHAR _NAMECHAR_any

PINAME                        ~ _NAME
PINAME_EXCEPTION              ~ 'xml':i

# The CDATA content using an exception would be:
#CDATACHARS                    ~ _CHAR_any
#CDATACHARS_EXCEPTION          ~ _CHAR_any ']]>' _CHAR_any

IGNORECHARS                   ~ _CHAR_any
IGNORECHARS_EXCEPTION         ~ _CHAR_any '<![' _CHAR_any
                              | _CHAR_any ']]>' _CHAR_any
CHAR_MINUS_GT                 ~ [\x{9}\x{A}\x{D}\x{20}-\x{3D}\x{3F}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u /* '>' is 0x3E */
CHAR_MINUS_RBRACKET           ~ [\x{9}\x{A}\x{D}\x{20}-\x{5C}\x{5E}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u /* ']' is 0x5D */
