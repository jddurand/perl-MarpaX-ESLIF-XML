# XML 1.0 (Fifth Edition) Grammar Personal Addendums

XML 1.0 (Fifth Edition) as per https://www.w3.org/TR/xml/ is **not** usable bindly for the two following reasons:

+ If you want to have unambiguous grammar valuation
+ Subtilities that do **not** appear in the rules (but elsewhere)

## Grammar ambiguities

### CharData

+ Reference: https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0000

    These rules make the grammar ambiguous:

```
[14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
[43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*

The fix is simple. Change:
[14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
to:
[14] CharData ::= [^<&]+ - ([^<&]* ']]>' [^<&]*)
```
### intSubset

+ Reference: https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001

```
[28a] DeclSep ::= PEReference | S
[28b] intSubset	::= (markupdecl | DeclSep)*
[3] S ::=  (#x20 | #x9 | #xD | #xA)+

intSubset is ambiguous because it allows repetitions (*) of DeclSep. And 
DeclSep can match the S rule which also allows repetitions (+).

I suggest the following correction, which appears to eliminate the 
ambiguity:

[3a] S1 ::=  #x20 | #x9 | #xD | #xA
[3b] S ::=  S1+
[28a] DeclSep ::= PEReference | S1
[28b] intSubset	::= (markupdecl | DeclSep)*
```

### Misc

+ Reference: https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002

```
[3] S ::= (#x20 | #x9 | #xD | #xA)+
[22] prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
[27] Misc ::= Comment | PI | S

If the prolog contains multiple consecutive space characters (S), then 
it's ambiguous how that should match some number of S, or some number of 
Misc containing S.

The fix for this ambiguity is similar to the previous one...

[27] Misc ::= Comment | PI | S1
[3a] S1 ::= #x20 | #x9 | #xD | #xA
[3b] S ::= S1+
```

## Subtilities

They all have to do with the allowed characters. The only allowed characters range is:

```
Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
```

and this is stated at the end of the grammar in the Notation section, e.g.:

[^a-z], [^#xN-#xN] matches any **Char** with a value outside the range indicated.

[^abc], [^#xN#xN#xN] matches any **Char** with a value not among the characters given. Enumerations and ranges of forbidden values can be mixed in one set of brackets.

The ranges that are not exclusions are not affected by this subtility.

So when later on the grammar says, e.g. for CharData: `[^<&]`, we _could_ read: any character but `<` or `&`. This is wrong: we **must** understand any **Char** but `<` or `&`. This mean that the following sections have to clarified:

### Literals

```
[9] EntityValue     ::= '"' ([^%&"] | PEReference | Reference)* '"' /* " == #x22, % == #x25, & == #x26 */
                      | "'" ([^%&'] | PEReference | Reference)* "'" /* % == #x25, & == #x26, ' == #x27 */
[10] AttValue       ::= '"' ([^<&"] | Reference)* '"'               /* " == #x22, % == #x25, & == #x26 */
                      | "'" ([^<&'] | Reference)* "'"               /* % == #x25, & == #x26, ' == #x27 */
[11] SystemLiteral  ::= '"' [^"]* '"'                               /* " == #x22 */
                      | "'" [^']* "'"                               /* ' == #x27 */

becomes:

[9a] EntityOrAttValueDQInner ::= #x9 | #xA | #xD | [#x20-#x21] | [#x23-#x24] | [#x27-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
[9b] EntityOrAttValueSQInner ::= #x9 | #xA | #xD | [#x20-#x24] |               [#x28-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]

[9] EntityValue              ::= '"' (EntityOrAttValueDQInner | PEReference | Reference)* '"'
                               | "'" (EntityOrAttValueSQInner | PEReference | Reference)* "'"
[10] AttValue                ::= '"' (EntityOrAttValueDQInner | Reference)* '"'
                               | "'" (EntityOrAttValueSQInner | Reference)* "'"

[11a] SystemLiteralDQInner   ::= #x9 | #xA | #xD | [#x20-#x21] | [#x23-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
[11b] SystemLiteralSQInner   ::= #x9 | #xA | #xD | [#x20-#x26] | [#x28-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]

[11] SystemLiteral           ::= '"' SystemLiteralDQInner* '"'
                               | "'" SystemLiteralSQInner* "'"
```

### Character Data

```
[14] CharData      ::= [^<&]+ - ([^<&]* ']]>' [^<&]*) /* & == #26, < == #x3c */


becomes:

[14a] CharDataUnit ::= #x9 | #xA | #xD | [#x20-#x25] | [#x26-#x3b] | [#x3d-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
[14] CharData      ::= CharDataUnit+ - (CharDataUnit* ']]>' CharDataUnit*)
```

