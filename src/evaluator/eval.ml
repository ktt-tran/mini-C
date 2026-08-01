open SmallCTypes
open Utils
open TokenTypes

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let lookup_b env x =
  let binds = env.bindings in
  let rec lookup lst =
  match lst with
    [] -> []
    | h::t ->
      let (id, value) = h in
      if id = x then [value]
      else lookup t
  in lookup binds
;;

let lookup_d env x =
  let lookup_defn = List.assoc_opt x env.defns in
  match lookup_defn with
  | None -> []
  | Some def -> [def]
;;

let lookup_all_p env x =
    let rec p_in_defns in_env =
      match in_env with
      | [] -> []
      | (name, body)::t ->
        let (_,params,_) = body in
        let lookup_p = List.assoc_opt x params in
        match lookup_p with
        | None -> p_in_defns t
        | Some par -> [par]
    in
    p_in_defns env.defns
;;

let add_bind env id v =
  { env with bindings = (id, v)::env.bindings}
;;

let update_b env id nv =
  let new_env = List.fold_left (fun acc x ->
    let (s,v) = x in
    if s = id then acc@[(s,nv)]
    else acc@[(s,v)]
  ) [] env.bindings
  in { env with bindings = new_env }
;;

(*create type check*)
let typecheck value typ =
  match value, typ with
  | Int_Val(x), Int_Type -> true
  | Bool_Val(x), Bool_Type -> true
  | _ -> false
;;

(*create bind*)
let rec bind_func old_env new_env params args =
  if (List.length params) <> (List.length args) then raise (TypeError "phi and alpha are different lengths")
  else
    (*old_env will contain all of the defns and bindings of the original environment
      new_env will contain all of the defns of the original environment and a fresh binding will be built with the new bindings*)
    let values = List.fold_left (fun acc alpha -> acc@[evaluate_expr old_env alpha]) [] args in
    let rec typecheck_params build_env phi alpha =
      match phi, alpha with
      | (id, typ)::p, v::a ->
        if typecheck v typ then
          typecheck_params { build_env with bindings = (id, v)::build_env.bindings} p a
        else raise (TypeError "phi and alpha do not contain the same types")
      | _, _ -> build_env
    in
    typecheck_params new_env params values

and evaluate_expr env t =
  match t with
  | Int x -> Int_Val(x)
  | Bool x -> Bool_Val(x)
  | ID x ->
    let search = lookup_b env x in
    if search = [] then raise (DeclareError "no ID found")
    else List.hd search
  | Add(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      let n3 = x1 + x2 in
      Int_Val(n3)
    | _ -> raise (TypeError "add"))
  | Sub(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      let n3 = x1 - x2 in
      Int_Val(n3)
    | _ -> raise (TypeError "sub"))
  | Mult(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      let n3 = x1 * x2 in
      Int_Val(n3)
    | _ -> raise (TypeError "mult"))
  | Div(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      if (x2 = 0) then raise (DivByZeroError)
      else
        let n3 = x1 / x2 in
        Int_Val(n3)
    | _ -> raise (TypeError "div"))
  | Pow(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      let n3 = float_of_int(x1) ** float_of_int(x2) in
      Int_Val(int_of_float(n3))
    | _ -> raise (TypeError "pow"))
  | Or(e1,e2) ->
    let b1 = evaluate_expr env e1 in
    let b2 = evaluate_expr env e2 in
    (match b1, b2 with
    | (Bool_Val x1), (Bool_Val x2) ->
      let b3 = x1 || x2 in
      Bool_Val(b3)
    | _ -> raise (TypeError "or"))
  | And(e1,e2) ->
    let b1 = evaluate_expr env e1 in
    let b2 = evaluate_expr env e2 in
    (match b1, b2 with
    | (Bool_Val x1), (Bool_Val x2) ->
      let b3 = x1 && x2 in
      Bool_Val(b3)
    | _ -> raise (TypeError "and"))
  | Not(e) ->
    let b1 = evaluate_expr env e in
    (match b1 with
    | Bool_Val x1 ->
      let b2 = not x1 in
      Bool_Val(b2)
    | _ -> raise (TypeError "or"))
  | Greater(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      let b = x1 > x2 in
      Bool_Val(b)
    | _ -> raise (TypeError "greater"))
  | Less(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      let b = x1 < x2 in
      Bool_Val(b)
    | _ -> raise (TypeError "less"))
  | GreaterEqual(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      let b = x1 >= x2 in
      Bool_Val(b)
    | _ -> raise (TypeError "greaterEqual"))
  | LessEqual(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      let b = x1 <= x2 in
      Bool_Val(b)
    | _ -> raise (TypeError "lessEqual"))
  | Equal(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      let b = (x1 = x2) in
      Bool_Val(b)
    | (Bool_Val x1), (Bool_Val x2) ->
      let b = (x1 = x2) in
      Bool_Val(b)
    | _ -> raise (TypeError "lessEqual"))
  | NotEqual(e1,e2) ->
    let n1 = evaluate_expr env e1 in
    let n2 = evaluate_expr env e2 in
    (match n1, n2 with
    | (Int_Val x1), (Int_Val x2) ->
      let b = (x1 <> x2) in
      Bool_Val(b)
    | (Bool_Val x1), (Bool_Val x2) ->
      let b = (x1 <> x2) in
      Bool_Val(b)
    | _ -> raise (TypeError "lessEqual"))
  | Call(c,e) ->
    let search_d = lookup_d env c in
    if search_d = [] then raise (DeclareError "defn does not exist")
    else let (typ, params, s) = List.hd search_d in
    let env' = bind_func env {defns = env.defns; bindings = []} params e in
    let env'' = evaluate_stmt env' s in
    let ret = lookup_b env'' "return" in
    let tcheck = typecheck (List.hd ret) typ in
    if tcheck then (List.hd ret) else raise (TypeError "type check error")

and evaluate_stmt env s =
  match s with
  | NoOp -> env
  | Seq(s1,s2) ->
    let env' = evaluate_stmt env s1 in
    let env'' = evaluate_stmt env' s2 in
    env''

  | Declare(typ,id) ->
    let search_bind = lookup_b env id in

    if search_bind <> []  then raise (DeclareError "declare")
    else
      (match typ with
      | Int_Type -> { env with bindings = (id, Int_Val(0))::env.bindings}
      | Bool_Type -> { env with bindings = (id, Bool_Val(false))::env.bindings })

  | Assign(id,e) ->
    let search = lookup_b env id in
    if search = [] then raise (DeclareError "assign variable")
    else
      let new_val = evaluate_expr env e in
      let cur_val = List.assoc id env.bindings in
      (match new_val, cur_val with
      | (Bool_Val(v1), Bool_Val(v2)) ->
        let env' = update_b env id new_val in
        env'
      | (Int_Val(v1), Int_Val(v2)) ->
        let env' = update_b env id new_val in
        env'
      | _ -> raise (TypeError "assign type"))
       
  | If(e,s1,s2) ->
    let guard = evaluate_expr env e in
    (match guard with
    | Bool_Val(true) -> evaluate_stmt env s1
    | Bool_Val(false) -> evaluate_stmt env s2
    | _ -> raise (TypeError "if"))

  | While(e,s) ->
      let guard = evaluate_expr env e in
      (match guard with
      | Bool_Val(true) ->
        let env' = evaluate_stmt env s in
        evaluate_stmt env' (While(e,s))
      | Bool_Val(false) -> env
      | _ -> raise (TypeError "while"))

  | For(id,e1,e2,s) ->
    let start = evaluate_expr env e1 in
    let fin = evaluate_expr env e2 in
    (match start, fin with
    | Int_Val(v1), Int_Val(v2) ->
      let env' = add_bind env id (Int_Val(v1)) in

      let rec for_helper env_arg =
        let pos_val = List.hd (lookup_b env_arg id) in
        (match pos_val with
          | Int_Val v3 -> 
            if v3 > v2 then env_arg
            else
              let env'' = update_b env_arg id (Int_Val(v3+1)) in
              let env''' = evaluate_stmt env'' s in
              for_helper env'''
          | _ -> raise (TypeError "for 1"))
          
      in for_helper env'
    | _,_ -> raise (TypeError "for2"))

  | Print e ->
    (*Needs work*)
    let expr = evaluate_expr env e in
    (match expr with
      | Int_Val(v) -> print_output_int v
      | Bool_Val(v) -> print_output_bool v);
      print_output_newline ();
    env

  | Return e ->
    let value = evaluate_expr env e in
    {env with bindings = ("return", value)::env.bindings}
;;

let rec evaluate env d =
  match d with
  | Multi(d1,d2) ->
    let env' = evaluate env d1 in
    let env'' = evaluate env' d2 in
    env''
  | Fun(typ, id, parameters, s) ->
      { env with defns = (id, (typ, parameters, s))::env.defns}
  | Main(s) ->
    let env' = evaluate_stmt env s in
    env'
;;

let rec eval_expr env t = evaluate_expr env t;;

let rec eval_stmt env s = evaluate_stmt env s;;

let rec eval env d = evaluate env d;;

let eval_or_none env expr = Some(eval env expr);;