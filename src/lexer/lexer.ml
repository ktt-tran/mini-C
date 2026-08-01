open TokenTypes

let tokenize input =

  (*define regular expression delimiters to split text code (except for '-')*)
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
      Re.set "+*/^"]) in

  let delim_lst = Re.split_full delim_reg input in

  (*discard all white space*)
  let clean_reg = Re.compile (Re.rep1 Re.space) in

  let input_lst = List.concat_map (function x ->
    match x with
    | `Delim d -> [Re.Group.get d 0]
    | `Text s -> Re.split clean_reg (String.trim s))
  delim_lst in

  (*helper to tokenize input*)
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

      (*Case N1-N2 -> N1 -N2*)
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
    
      (*Case -N1-N2 -> -N1 -N2*)
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

      (*Case N-*)
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

      (*Case -N-*)
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

      (*Valid Tok id*)
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
