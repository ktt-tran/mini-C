(* The first section of the lexer definition, called the *header*,
   is the part that appears below between { and }.  It is code
   that will simply be copied literally into the generated lexer.ml. *)

{
open Parser

open TokenTypes

let tokenize input =

  (*define regular expression delimiters to split text code*)
  let delim_reg = Re.compile (
    Re.alt [
      Re.set "(){}"; 
      Re.str "=="; 
      Re.str "!="; 
      Re.str ">="; 
      Re.str "<="; 
      Re.set "><"; 
      Re.str "="; 
      Re.str "||"; 
      Re.str "&&"; 
      Re.set "!;,"; 
      Re.set "+*/^"]) (*special cases will be handled for '-'*)
    in 

  (* create list of defined expressions *)
  let delim_lst = Re.split_full delim_reg input in

  (*discard all white space*)
  let clean_reg = Re.compile (Re.rep1 Re.space) in
  
  let input_lst = List.concat_map (function x ->
    match x with
    | `Delim d -> [Re.Group.get d 0]
    | `Text s -> Re.split clean_reg (String.trim s))
  delim_lst in

  (*Take cleaned input string and tokenize each element as tokens*)
  let rec tokenize_helper lst token_lst =
  match lst with
  | [] -> token_lst@[EOF]
  | h::t ->
    match h with
    | "(" -> tokenize_helper t token_lst@[Tok_LParen]
    | ")" -> tokenize_helper t token_lst@[Tok_RParen]
    | "{" -> tokenize_helper t token_lst@[Tok_LBrace]
    | "}" -> tokenize_helper t token_lst@[Tok_RBrace]
    | "==" -> tokenize_helper t token_lst@[Tok_Equal]
    | "!=" -> tokenize_helper t token_lst@[Tok_NotEqual]
    | "=" -> tokenize_helper t token_lst@[Tok_Assign]
    | ">" -> tokenize_helper t token_lst@[Tok_Greater]
    | "<" -> tokenize_helper t token_lst@[Tok_Less]
    | ">=" -> tokenize_helper t token_lst@[Tok_GreaterEqual]
    | "<=" -> tokenize_helper t token_lst@[Tok_LessEqual]
    | "||" -> tokenize_helper t token_lst@[Tok_Or]
    | "&&" -> tokenize_helper t token_lst@[Tok_And]
    | "!" -> tokenize_helper t token_lst@[Tok_Not]
    | ";" -> tokenize_helper t token_lst@[Tok_Semi]
    | "," -> tokenize_helper t token_lst@[Tok_Comma]
    | "int" -> tokenize_helper t token_lst@[Tok_Int_Type]
    | "bool" -> tokenize_helper t token_lst@[Tok_Bool_Type]
    | "printf" -> tokenize_helper t token_lst@[Tok_Print]
    | "main" -> tokenize_helper t token_lst@[Tok_Main]
    | "if" -> tokenize_helper t token_lst@[Tok_If]
    | "else" -> tokenize_helper t token_lst@[Tok_Else]
    | "for" -> tokenize_helper t token_lst@[Tok_For]
    | "from" -> tokenize_helper t token_lst@[Tok_From]
    | "to" -> tokenize_helper t token_lst@[Tok_To]
    | "while" -> tokenize_helper t token_lst@[Tok_While]
    | "return" -> tokenize_helper t token_lst@[Tok_Return]
    | "+" -> tokenize_helper t token_lst@[Tok_Add]
    | "-" -> tokenize_helper t token_lst@[Tok_Sub]
    | "*" -> tokenize_helper t token_lst@[Tok_Mult]
    | "/" -> tokenize_helper t token_lst@[Tok_Div]
    | "^" -> tokenize_helper t token_lst@[Tok_Pow]
    | "true" -> tokenize_helper t token_lst@[Tok_Bool(true)]
    | "false" -> tokenize_helper t token_lst@[Tok_Bool(false)]
    | _ ->
      (*Tok number*)
      if Re.execp (
        Re.compile (
          Re.whole_string (
            Re.seq [
              Re.opt (Re.char '-'); Re.rep1 Re.digit])) ) h 
              then tokenize_helper t token_lst@[Tok_Int( int_of_string h )] else

      (*handle special cases with subtraction and negative depending which can change depending on the
       order and placement of '-'*)

      (*case N1-N2 -> N1 -N2*)
      if Re.execp (
        Re.compile (
          Re.whole_string (
            Re.seq [Re.rep1 Re.digit; Re.rep1 ( Re.seq [(Re.char '-'); Re.rep1 Re.digit] ) ])) ) h then
        let expr = Re.split ( Re.compile (Re.char '-')) h in

        let rec sub_case expr_l lst =
          match expr_l with
          [] -> lst
          | l::[] -> lst@[Tok_Int( int_of_string l )]
          | l::r -> sub_case r lst@[Tok_Int( -(int_of_string l ))]

        in

        tokenize_helper t token_lst@(List.rev(sub_case expr [])) else
    
      (*case -N1-N2 -> -N1 -N2*)
      if Re.execp (
        Re.compile (
          Re.whole_string (
            Re.seq [Re.char '-'; Re.rep1 Re.digit; Re.rep1 ( Re.seq [(Re.char '-'); Re.rep1 Re.digit] ) ])) ) h then
        let expr = Re.split ( Re.compile (Re.char '-')) h in

        let rec sub_case expr_l lst =
          match expr_l with
          [] -> lst
          | l::[] -> lst@[Tok_Int( -( int_of_string l ) )]
          | l::r -> sub_case r lst@[Tok_Int( -(int_of_string l ) )]

        in

        tokenize_helper t token_lst@(List.rev(sub_case expr [])) else

      (*case N-*)
      if Re.execp (
        Re.compile (
          Re.whole_string (Re.rep1 ( Re.seq [Re.rep1 Re.digit; Re.char '-'] ))) ) h then
        let expr = Re.split ( Re.compile (Re.char '-')) h in

        let rec sub_case expr_l lst =
          match expr_l with
          [] -> lst
          | l::[] -> lst@[Tok_Int( ( int_of_string l ) )]
          | l::r -> sub_case r lst@[Tok_Int( -(int_of_string l ) )]

        in

        tokenize_helper t token_lst@([Tok_Sub]@(List.rev (sub_case expr []))) else

      (*case -N-*)
      if Re.execp (
        Re.compile (
          Re.whole_string (
            Re.seq [Re.char '-'; Re.rep1 ( Re.seq [Re.rep1 Re.digit; Re.char '-'] )])) ) h then
        let expr = Re.split ( Re.compile (Re.char '-')) h in

        let rec sub_case expr_l lst =
          match expr_l with
          [] -> lst
          | l::[] -> lst@[Tok_Int( -( int_of_string l ) )]
          | l::r -> sub_case r lst@[Tok_Int( -(int_of_string l ) )]

        in

        tokenize_helper t token_lst@([Tok_Sub]@(List.rev (sub_case expr []))) else

      (*validate ID*)
      (*valid Tok id*)
      if Re.execp (
        Re.compile (
          Re.whole_string (
            Re.seq [Re.alt [Re.lower; Re.upper]; Re.rep (Re.alt [Re.lower; Re.upper; Re.digit])])) ) h then
        tokenize_helper t token_lst@[Tok_ID(h)]
      else
        tokenize_helper t token_lst

  in

  List.rev (tokenize_helper input_lst [])
;;

(* (, ), {, }, ==, !=, =, >, <, >=, <=, ||, &&, !, ;, \, , +, -, *, /, ^ *)
(* Can be placed right next to another string *)

(* (, {, ==, !=, =, >, <, >=, <=, ||, &&, !, ;, \, , +, -, *, /, ^ *)
(* must have something come after it *)

(* ), } *)
(* must have something come before it *)

(*int, bool, main, if, else, for, from, to, while, return, true, alse *)
(* needs to be checked for longest string *)

(*check if the following is followed by a '(' :
  (, {, ==, !=, =, >, <, >=, <=, ||, &&, !, +, -, *, /, ^, if, else, for, from, to, while, return*)

}

(* The second section of the lexer definition defines *identifiers*
   that will be used later in the definition.  Each identifier is
   a *regular expression*.

   Below, are the defined regular expressions for
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

(* The final section of the lexer definition defines parsing a character
   stream into a token stream. Each of the rules below has the form
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