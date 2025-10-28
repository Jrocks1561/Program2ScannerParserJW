Scanner spec (exact tokens to recognize)

Keywords: if then else while begin end read print

Ops/punct: + - * / := ( ) ;

Comparators: = > < >= <= <>

IDs: alpha (alnum | _ | -)*

Numbers:

INT: optional sign; if >1 digit then first digit ≠ 0

FLOAT: optional sign; digits '.' digits (must have ≥1 digit after dot; 3. invalid, 3.0 valid)

No scientific/hex/oct/bin

Structure: NEWLINE at each physical line end; EOF at end

Comments: /* … */ (first */ closes; can span lines; not nested) → skip

Whitespace: skip (except NEWLINE emitted as a token)

Include line and column on each token for clear errors.

Parser checklist (one function per rule)

program → stmt-list EOF

stmt-list → stmt stmt-list | ε

Accept NEWLINE between statements

stmt → if-stmt | while-stmt | assign-stmt | read-stmt | print-stmt | compound-stmt

compound-stmt → begin stmt { ; stmt }* end

if-stmt → if expr comp-op expr then begin stmt-list end [ else begin stmt-list end ]

while-stmt → while expr comp-op expr begin stmt-list end

assign-stmt → ID := expr

read-stmt → read ID

print-stmt → print expr

Expressions (enforce precedence & no line breaks):

expr → term + expr | term - expr | term comp-op term | term

term → factor * term | factor / term | factor

factor → ID | num | ( expr )

If NEWLINE appears before an expression is complete → error: “expression cannot cross a line break”