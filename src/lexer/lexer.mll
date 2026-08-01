(* The first section of the lexer definition, called the *header*,
   is the part that appears below between { and }.  It is code
   that will simply be copied literally into the generated lexer.ml. *)

{
open Parser
}

(* The second section of the lexer definition defines *identifiers*
   that will be used later in the definition.  Each identifier is
   a *regular expression*.  We won't go into details on how regular
   expressions work.

   Below, we define regular expressions for
     - whitespace (spaces, tabs, and newlines),
     - digits (0 through 9),
     - integers (nonempty sequences of digits, optionally preceded by a minus sign),
     - letters (a through z, and A through Z), and
     - identifiers (letter followed by any number of letters or digits). *)

let blank = [' ' '\t' '\n']+
let decimal_literal = ['0'-'9']+
let neg_int = '-' ['0'-'9']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter (letter | decimal_literal)*

(* The final section of the lexer definition defines how to parse a character
   stream into a token stream.  Each of the rules below has the form
     | regexp { action }
   If the lexer sees the regular expression [regexp], it produces the token
   specified by the [action].

   Keywords appear before the `id` rule so that ties (same length) resolve
   to the keyword token.  Longer matches still win regardless of order, so
   "while0" correctly lexes as IDENT "while0" instead of WHILE + INT 0. *)

rule token =
  parse
  | blank      { token lexbuf }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | "{"        { LBRACE }
  | "}"        { RBRACE }
  | "=="       { EQUALS }
  | "!="       { NE }
  | ">="       { GE }
  | "<="       { LE }
  | ">"        { GT }
  | "<"        { LT }
  | "="        { ASSIGNS }
  | "||"       { OR }
  | "&&"       { AND }
  | "!"        { NOT }
  | ";"        { SEMI }
  | ","        { COMMA }
  | "printf"   { PRINT }
  | "main"     { MAIN }
  | "if"       { IF }
  | "else"     { ELSE }
  | "for"      { FOR }
  | "from"     { FROM }
  | "to"       { TO }
  | "while"    { WHILE }
  | "return"   { RETURN }
  | "int"      { INTTYPE }
  | "bool"     { BOOLTYPE }
  | "false"    { BOOL false }
  | "true"     { BOOL true }
  | "+"        { PLUS }
  | neg_int    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "-"        { MINUS }
  | "*"        { MULT }
  | "/"        { DIV }
  | "^"        { POW }
  | id         { IDENT (Lexing.lexeme lexbuf) }
  | decimal_literal { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof        { EOF }

(* And that's the end of the lexer definition. *)
