open MiniCTypes


type var_env = (string * data_type) list
type fun_env = (string * (data_type * (string * data_type) list)) list


let lookup_v env x =
  let lookup_var = List.assoc_opt x env in
  match lookup_var with
  | None -> []
  | Some dt -> [dt]
;;

let lookup_f env x =
  let lookup_fun = List.assoc_opt x env in
  match lookup_fun with
  | None -> raise (DeclareError "fun not found")
  | Some f -> f
;;

(*create type check*)
let typchk value typ =
  match value, typ with
  | Int_Type, Int_Type -> true
  | Bool_Type, Bool_Type -> true
  | _ -> false
;;


let rec typecheck_expr (fenv : fun_env) (venv : var_env) expr =
  match expr with
  | Int x -> Int_Type
  | Bool x -> Bool_Type
  | ID x ->
    let search = lookup_v venv x in
    if search = [] then raise (DeclareError "unbound variable")
    else List.hd search
  | Add(e1,e2) ->
    let n1 = typecheck_expr fenv venv e1 in
    let n2 = typecheck_expr fenv venv e2 in
    (match n1, n2 with
    | Int_Type, Int_Type -> Int_Type
    | _ -> raise (TypeError "add"))
  | Sub(e1,e2) ->
    let n1 = typecheck_expr fenv venv e1 in
    let n2 = typecheck_expr fenv venv e2 in
    (match n1, n2 with
    | Int_Type, Int_Type -> Int_Type
    | _ -> raise (TypeError "sub"))
  | Mult(e1,e2) ->
    let n1 = typecheck_expr fenv venv e1 in
    let n2 = typecheck_expr fenv venv e2 in
    (match n1, n2 with
    | Int_Type, Int_Type -> Int_Type
    | _ -> raise (TypeError "mult"))
  | Div(e1,e2) ->
    let n1 = typecheck_expr fenv venv e1 in
    let n2 = typecheck_expr fenv venv e2 in
    (match n1, n2 with
    | Int_Type, Int_Type -> Int_Type
    | _ -> raise (TypeError "div"))
  | Pow(e1,e2) ->
    let n1 = typecheck_expr fenv venv e1 in
    let n2 = typecheck_expr fenv venv e2 in
    (match n1, n2 with
    | Int_Type, Int_Type -> Int_Type
    | _ -> raise (TypeError "pow"))
  | Or(e1,e2) ->
    let b1 = typecheck_expr fenv venv e1 in
    let b2 = typecheck_expr fenv venv e2 in
    (match b1, b2 with
    | Bool_Type, Bool_Type -> Bool_Type
    | _ -> raise (TypeError "or"))
  | And(e1,e2) ->
    let b1 = typecheck_expr fenv venv e1 in
    let b2 = typecheck_expr fenv venv e2 in
    (match b1, b2 with
    | Bool_Type, Bool_Type -> Bool_Type
    | _ -> raise (TypeError "and"))
  | Not(e) ->
    let b = typecheck_expr fenv venv e in
    (match b with
    | Bool_Type -> Bool_Type
    | _ -> raise (TypeError "or"))
  | Greater(e1,e2) ->
    let t1 = typecheck_expr fenv venv e1 in
    let t2 = typecheck_expr fenv venv e2 in
    if t1 = t2 then Bool_Type
    else raise (TypeError "greater")
  | Less(e1,e2) ->
    let t1 = typecheck_expr fenv venv e1 in
    let t2 = typecheck_expr fenv venv e2 in
    if t1 = t2 then Bool_Type
    else raise (TypeError "less")
  | GreaterEqual(e1,e2) ->
    let t1 = typecheck_expr fenv venv e1 in
    let t2 = typecheck_expr fenv venv e2 in
    if t1 = t2 then Bool_Type
    else raise (TypeError "greaterEqual")
  | LessEqual(e1,e2) ->
    let t1 = typecheck_expr fenv venv e1 in
    let t2 = typecheck_expr fenv venv e2 in
    if t1 = t2 then Bool_Type
    else raise (TypeError "lessEqual")
  | Equal(e1,e2) ->
    let t1 = typecheck_expr fenv venv e1 in
    let t2 = typecheck_expr fenv venv e2 in
    if t1 = t2 then Bool_Type
    else raise (TypeError "Equal")
  | NotEqual(e1,e2) ->
    let t1 = typecheck_expr fenv venv e1 in
    let t2 = typecheck_expr fenv venv e2 in
    if t1 = t2 then Bool_Type
    else raise (TypeError "NotEqual")
  | Call(c,e) ->
    let (ret_typ, params) = lookup_f fenv c in
    let rec pass_tcheck params args =
      (match params, args with
      | [], [] -> ()
      | (_,t)::r_params, exp::r_expr ->
        let t_ae = typecheck_expr fenv venv exp in
        if t_ae <> t then raise (TypeError "call - type mismatch")
        else pass_tcheck r_params r_expr
      | _, _ -> raise (TypeError "call - error 2"))
    in
    pass_tcheck params e;
    ret_typ

and typecheck_stmt_helper fenv venv stmts =
  match stmts with
  | [] -> venv
  | h::t ->
    let venv' = typecheck_stmt fenv venv h in
    typecheck_stmt_helper fenv venv' t

and typecheck_stmt (fenv : fun_env) (venv : var_env) stmt =
  match stmt with
  | Declare(t0,id) ->
    let search = lookup_v venv id in
    if search <> [] then raise (DeclareError "declare ID already exist")
    else
      (id, t0)::venv

  | Assign(id,e) ->
    let t0 = lookup_v venv id in
    if t0 = [] then raise (DeclareError "assign variable")
    else
      let t1 = typecheck_expr fenv venv e in
	    if t1 = List.hd(t0) then (id, List.hd(t0))::venv
      else raise (TypeError "assign type mismatch")
       
  | If(e,s1,s2) ->
    let t0_guard = typecheck_expr fenv venv e in
    (match t0_guard with
    | Bool_Type -> 
      let venv' = typecheck_stmt_helper fenv venv s1 in
      let venv'' = typecheck_stmt_helper fenv venv s2 in
      venv' @ venv''
    | _ -> raise (TypeError "if - guard must resolve to bool"))

  | While(e,s) ->
    let t0_guard = typecheck_expr fenv venv e in
    (match t0_guard with
    | Bool_Type ->
      let venv' = typecheck_stmt_helper fenv venv s in
      venv'
    | _ -> raise (TypeError "while - guard must resolve to bool"))

  | For(id,e1,e2,s) ->
    let t0_predef = lookup_v venv id in
    if t0_predef <> [] then
      let t2 = typecheck_expr fenv venv e1 in
      let t3 = typecheck_expr fenv venv e2 in
      (match List.hd(t0_predef), t2, t3 with
      | Int_Type, Int_Type, Int_Type -> typecheck_stmt_helper fenv venv s
      | _ -> raise (TypeError "for - predef"))
    else 
      let t2 = typecheck_expr fenv venv e1 in
      let t3 = typecheck_expr fenv venv e2 in
      let venv' = (id, Int_Type)::venv in
      (match t2, t3 with
      | Int_Type, Int_Type -> typecheck_stmt_helper fenv venv' s
      | _ -> raise (TypeError "for - not predef"))
	
  | Print(e) ->
    (*Needs work*)
    let _t0 = typecheck_expr fenv venv e in
    venv

  | Return e ->
    let t = typecheck_expr fenv venv e in
    ("return", t)::venv

and typecheck_defn_helper fenv defn =
  match defn with
  | Fun (f_def) -> typecheck_stmt_helper fenv f_def.params f_def.body
  | Main(s) -> typecheck_stmt_helper fenv [] s
  | _ -> []

and typecheck_defn defn =
  match defn with
  | Defns(defn_list) ->
  let fenv = List.fold_left (fun acc defn_f ->
    match defn_f with
    | Fun (f_def) -> (f_def.name, (f_def.ret_type, f_def.params))::acc
    | _ -> acc
  ) [] defn_list in
    
    let _typ = List.fold_left (
      fun acc defn -> (
        typecheck_defn_helper fenv defn)::acc) [] defn_list in
    true

  | Fun (f_def) ->
    let fenv = [f_def.name, (f_def.ret_type, f_def.params)] in
    let venv = f_def.params in
    let _venv' = typecheck_stmt_helper fenv venv f_def.body in
    true
  | Main(s) ->
    let fenv = [] in
    let venv = [] in
    let _venv' = typecheck_stmt_helper fenv venv s in
    true


let rec typecheck stmts =
  let _typ_stmts = typecheck_stmt_helper [] [] stmts in
  true
;;
