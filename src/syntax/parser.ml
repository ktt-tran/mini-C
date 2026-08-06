open MiniCTypes
open Utils
open TokenTypes

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(*Parser for expressions and keywords*)
let rec parse_Expr tokens =
  let (tok, e) = parse_Or tokens in
  (tok, e)


and parse_Or tokens =
  let (t1, e1) = parse_And tokens in
  match lookahead t1 with
  | Tok_Or ->
    let t2 = match_token t1 Tok_Or in
    let t3, e2 = parse_Or t2 in
    (t3, Or (e1, e2))
  | _ -> (t1, e1)


and parse_And tokens =
  let (t1, e1) = parse_Equality tokens in
  match lookahead t1 with
  | Tok_And ->
    let t2 = match_token t1 Tok_And in
    let t3, e2 = parse_And t2 in
    (t3, And (e1, e2))
  | _ -> (t1, e1)

    
and parse_Equality tokens =
  let (t1, e1) = parse_Relational tokens in
  match lookahead t1 with
  | Tok_Equal ->
    let t2 = match_token t1 Tok_Equal in
    let t3, e2 = parse_Equality t2 in
    (t3, Equal (e1, e2))
  | Tok_NotEqual ->
    let t2 = match_token t1 Tok_NotEqual in
    let t3, e2 = parse_Equality t2 in
    (t3, NotEqual (e1, e2))
  | _ -> (t1, e1)


and parse_Relational tokens =
  let (t1, e1) = parse_Additive tokens in
  match lookahead t1 with
  | Tok_LessEqual ->
    let t2 = match_token t1 Tok_LessEqual in
    let t3, e2 = parse_Relational t2 in
    (t3, LessEqual (e1, e2))
  | Tok_GreaterEqual ->
    let t2 = match_token t1 Tok_GreaterEqual in
    let t3, e2 = parse_Relational t2 in
    (t3, GreaterEqual (e1, e2))
  | Tok_Less ->
    let t2 = match_token t1 Tok_Less in
    let t3, e2 = parse_Relational t2 in
    (t3, Less (e1, e2))
  | Tok_Greater ->
    let t2 = match_token t1 Tok_Greater in
    let t3, e2 = parse_Relational t2 in
    (t3, Greater (e1, e2))
  | _ -> (t1, e1)


and parse_Additive tokens =
  let (t1, e1) = parse_Multiplicative tokens in
  match lookahead t1 with
  | Tok_Add ->
    let t2 = match_token t1 Tok_Add in
    let t3, e2 = parse_Additive t2 in
    (t3, Add (e1, e2))
  | Tok_Sub ->
    let t2 = match_token t1 Tok_Sub in
    let t3, e2 = parse_Additive t2 in
    (t3, Sub (e1, e2))
  | _ -> (t1, e1)


and parse_Multiplicative tokens =
  let (t1, e1) = parse_Power tokens in
  match lookahead t1 with
  | Tok_Mult ->
    let t2 = match_token t1 Tok_Mult in
    let t3, e2 = parse_Multiplicative t2 in
    (t3, Mult (e1, e2))
  | Tok_Div ->
    let t2 = match_token t1 Tok_Div in
    let t3, e2 = parse_Multiplicative t2 in
    (t3, Div (e1, e2))
  | _ -> (t1, e1)


and parse_Power tokens =
  let (t1, e1) = parse_Unary tokens in
  match lookahead t1 with
  | Tok_Pow ->
    let t2 = match_token t1 Tok_Pow in
    let t3, e2 = parse_Power t2 in
    (t3, Pow (e1, e2))
  | _ -> (t1, e1)


and parse_Unary tokens =
  match lookahead tokens with
  | Tok_Not ->
    let t1 = match_token tokens Tok_Not in
    let t2, e1 = parse_Unary t1 in
    (t2, Not e1)
  | _ -> parse_Call tokens


and parse_Call tokens =
  match tokens with
  | (Tok_ID c)::(Tok_LParen)::t ->
    let t1 = match_token tokens (Tok_ID c) in
    (match lookahead t1 with
    |Tok_LParen ->
      let t2 = match_token t1 Tok_LParen in
      let t3, e1 = parse_Arg t2 in
      if lookahead t3 = Tok_RParen then
        (match_token t3 Tok_RParen, Call(c, e1))
      else raise (InvalidInputException "parse_Call 1")
    | _ -> raise (InvalidInputException "parse_Call 2"))
  | _ -> parse_PrimaryExpr tokens


(* CallExpr →\rightarrow→ Tok_ID ( Args ) | PrimaryExpr


Args →\rightarrow→ List | ε
List →\rightarrow→ Expr , List | Expr *)


and parse_Arg tokens =
  let t1, e1 = parse_List tokens in
  (t1, e1)
  

and parse_List tokens =
  let t1, e1 = parse_Expr tokens in
  match lookahead t1 with
  | Tok_Comma ->
    let t2 = match_token t1 Tok_Comma in
    let t3, e2 = parse_List t2 in
    (t3, e1::e2)
  | _ -> (t1, e1::[])


and parse_PrimaryExpr tokens =
  match lookahead tokens with
  | Tok_Int c ->
    let t = match_token tokens (Tok_Int c) in
    (t, Int c)
  | Tok_Bool c ->
    let t = match_token tokens (Tok_Bool c) in
    (t, Bool c)
  | Tok_ID c ->
    let t = match_token tokens (Tok_ID c) in
    (t, ID c)
  | Tok_LParen ->
    let t1 = match_token tokens Tok_LParen in
    let t2, e = parse_Expr t1 in
    if lookahead t2 = Tok_RParen then ((match_token t2 Tok_RParen), e)
    else raise (InvalidInputException "parse_Primary 1")
  | _ -> raise (InvalidInputException "parse_Primary 2");;


  (*Parser for statements when function*)
let rec parse_Stmt tokens =
  let t1, s1 = parse_Comp tokens in
  let t2, s2 = parse_Return t1 in
  (t2, Seq (s1, s2))

and parse_Comp tokens =
  match lookahead tokens with
	| Tok_Int_Type 
    | Tok_Bool_Type 
    | Tok_ID(_) 
    | Tok_Assign 
    | Tok_Print 
    | Tok_If 
    | Tok_For 
    | Tok_While ->
    let t1, s1 = parse_compStmt tokens in
    let t2, s2 = parse_Comp t1 in
    (t2, Seq(s1, s2))
  | _ -> (tokens, NoOp)


and parse_compStmt tokens =
	match lookahead tokens with
	| Tok_Int_Type ->
		let t, s = parse_Declare tokens in
		(t, s)
	| Tok_Bool_Type -> 
		let t, s = parse_Declare tokens in
		(t, s)
  | Tok_ID c ->
    let t, s = parse_Assign tokens in
		(t, s)
	| Tok_Print ->
		let t, s = parse_Print tokens in
		(t, s)
	| Tok_If ->
		let t, s = parse_If tokens in
		(t, s)
	| Tok_For ->
		let t, s = parse_For tokens in
		(t, s)
	| Tok_While ->
		let t, s = parse_While tokens in
		(t, s)
  | _ -> (tokens, NoOp)


and parse_Declare tokens =
	let t1, typ = parse_BasicType tokens in
	match lookahead t1 with
	| Tok_ID c ->
		let t2 = match_token t1 (Tok_ID c) in
		let t3 = match_token t2 Tok_Semi in
		(t3, Declare (typ,  c))
	| _ -> raise (InvalidInputException "parse_Declare")


and parse_BasicType tokens =
	match lookahead tokens with
	| Tok_Int_Type -> ((match_token tokens Tok_Int_Type), Int_Type)
	| Tok_Bool_Type -> ((match_token tokens Tok_Bool_Type), Bool_Type)
	| _ -> raise (InvalidInputException "parse_BasicType 1")

and parse_Assign tokens =
	match lookahead tokens with
	| Tok_ID c ->
		let t1 = match_token tokens (Tok_ID c) in
		let t2 = match_token t1 Tok_Assign in
		let t3, e = parse_Expr t2 in
		let t4 = match_token t3 Tok_Semi in
		(t4, Assign(c, e))
	| _ -> raise (InvalidInputException "parse_Assign")


and parse_Print tokens =
	match lookahead tokens with
	| Tok_Print ->
		let t1 = match_token tokens Tok_Print in
		let t2 = match_token t1 Tok_LParen in
		let t3, e = parse_Expr t2 in
		let t4 = match_token t3 Tok_RParen in
		let t5 = match_token t4 Tok_Semi in
		(t5, Print e)
| _ -> raise (InvalidInputException "parse_Print")


and parse_If tokens =
	let t1 = match_token tokens Tok_If in
	let t2 = match_token t1 Tok_LParen in
	let t3, e = parse_Expr t2 in
	let t4 = match_token t3 Tok_RParen in
	let t5 = match_token t4 Tok_LBrace in
	let t6, s1 = parse_Comp t5 in
	let t7 = match_token t6 Tok_RBrace in
	let t8, s2 = parse_Else t7 in
	(t8, If (e, s1, s2))


and parse_Else tokens =
  match lookahead tokens with
  | Tok_Else ->
    let t1 = match_token tokens Tok_Else in
    let t2 = match_token t1 Tok_LBrace in
    let t3, s = parse_Comp t2 in
    let t4 = match_token t3 Tok_RBrace in
    (t4, s)
  | _ ->
    (tokens, NoOp)


and parse_For tokens =
	let t1 = match_token tokens Tok_For in
  let t2 = match_token t1 Tok_LParen in
	match lookahead t2 with
	| Tok_ID c ->
		let t3 = match_token t2 (Tok_ID c) in
		let t4 = match_token t3 Tok_From in
		let t5, e1 = parse_Expr t4 in
		let t6 = match_token t5 Tok_To in
		let t7, e2 = parse_Expr t6 in
		let t8 = match_token t7 Tok_RParen in
		let t9 = match_token t8 Tok_LBrace in
		let t10, s = parse_Comp t9 in
		let t11 = match_token t10 Tok_RBrace in
		(t11, For (c, e1, e2, s))
| _ -> raise (InvalidInputException "parse_For")


and parse_While tokens =
	let t1 = match_token tokens Tok_While in
  let t2 = match_token t1 Tok_LParen in
  let t3, e = parse_Expr t2 in
	let t4 = match_token t3 Tok_RParen in
  let t5 = match_token t4 Tok_LBrace in
  let t6, s = parse_Comp t5 in
	let t7 = match_token t6 Tok_RBrace in
	(t7, While (e, s))


and parse_Return tokens =
  match lookahead tokens with
  | Tok_Return ->
    let t1 = match_token tokens Tok_Return in
    let t2, e = parse_Expr t1 in
    let t3 = match_token t2 Tok_Semi in
    (t3, Return e)
  | _ -> raise (InvalidInputException "parse_Return");;


(*Parser for definitions, function, and data structures*)
(*lookahead first two tokens for int main*)
let rec parse_File tokens =
  match tokens with
  | h1::h2::t1 when h1 = Tok_Int_Type && h2 = Tok_Main ->
    let t2, d = parse_Main tokens in
    (t2, d)
  | _ ->
    let t1, d1 = parse_Defs tokens in
      if lookahead t1 = EOF then (t1, d1)
      else
      let t2, d2 = parse_Main t1 in
      (t2, Multi(d1, d2))


and parse_Defs tokens =
  let t1, d1 = parse_Fun tokens in
  match t1 with
  | Tok_Int_Type::Tok_Main::t -> (t1, d1)
  | Tok_Int_Type::t ->
    let t2, d2 = parse_Defs t1 in
    (t2, Multi(d1, d2))
  | Tok_Bool_Type::t ->
    let t2, d2 = parse_Defs t1 in
    (t2, Multi(d1, d2))
  | _ -> 
    (t1, d1)


and parse_Fun tokens =
	let t1, typ = parse_BasicTypea tokens in
	match lookahead t1 with
	| Tok_ID c ->
		let t2 = match_token t1 (Tok_ID c) in
		let t3 = match_token t2 Tok_LParen in
		let t4, arg = parse_Arg t3 in
		let t5 = match_token t4 Tok_RParen in
		let t6 = match_token t5 Tok_LBrace in
		let t7, s = parse_Stmt t6 in
		let t8 = match_token t7 Tok_RBrace in
		(t8, Fun (typ, c, arg, s))
  | _ -> raise (InvalidInputException "parse_Fun")


and parse_Arg tokens =
	let t, lst = parse_List tokens in
	(t, lst)


and parse_List tokens =
  let t1, typ = parse_BasicTypeb tokens in
  match lookahead t1 with
  | Tok_ID c ->
    let t2 = match_token t1 (Tok_ID c) in
    (match lookahead t2 with
    | Tok_Comma ->
      let t3 = match_token t2 Tok_Comma in
      let t4, lst = parse_List t3 in
      (t4, (c, typ)::lst)
    | _ -> (t2, [(c, typ)]))
  | _ -> raise (InvalidInputException "parse_Arg")


and parse_BasicTypea tokens =
	match lookahead tokens with
	| Tok_Int_Type -> ((match_token tokens Tok_Int_Type), Int_Type)
	| Tok_Bool_Type -> ((match_token tokens Tok_Bool_Type), Bool_Type)
	| _ -> raise (InvalidInputException "parse_BasicType a")

and parse_BasicTypeb tokens =
	match lookahead tokens with
	| Tok_Int_Type -> ((match_token tokens Tok_Int_Type), Int_Type)
	| Tok_Bool_Type -> ((match_token tokens Tok_Bool_Type), Bool_Type)
	| _ -> raise (InvalidInputException "parse_BasicType b")


and parse_Main tokens =
	let t1 = match_token tokens Tok_Int_Type in
	let t2 = match_token t1 Tok_Main in
	let t3 = match_token t2 Tok_LParen in
	let t4 = match_token t3 Tok_RParen in
	let t5 = match_token t4 Tok_LBrace in
	let t6, s = parse_Stmt t5 in
	let t7 = match_token t6 Tok_RBrace in
	let t8 = match_token t7 EOF in
	(t8, Main s);;

  
let rec parse_expr toks : token list * expr =
  parse_Expr toks;;

let rec parse_stmt toks : token list * stmt =
  parse_Stmt toks;;

let rec parse_defs toks : token list * defn =
  parse_File toks;;