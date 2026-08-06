(* Stack Machine Instructions *)
type instr =
  (* Literals: booleans are encoded as integers (0 = false, 1 = true) *)
  | Push of int              (* push integer literal onto the stack *)

  (* Variables *)
  | Load of string           (* push value of named variable onto stack *)
  | Store of string          (* pop top of stack, write into named variable *)
  | Declare of string        (* declare variable; initializes to 0 *)

  (* Arithmetic — pops n2 then n1, pushes result *)
  | Add                      (* n1 + n2 *)
  | Sub                      (* n1 - n2 *)
  | Mul                      (* n1 * n2 *)
  | Div                      (* n1 / n2 *)
  | Pow                      (* n1 ^ n2 *)

  (* Comparisons — pops n2 then n1, pushes 1 (true) or 0 (false) *)
  | Lt                       (* n1 < n2 *)
  | Gt                       (* n1 > n2 *)
  | Lte                      (* n1 <= n2 *)
  | Gte                      (* n1 >= n2 *)
  | Eq                       (* v1 = v2  (works for both int and bool) *)
  | Neq                      (* v1 <> v2 *)

  (* Boolean — pops b2 then b1 (as 0/1 ints), pushes 0/1 result *)
  | And                      (* b1 && b2 *)
  | Or                       (* b1 || b2 *)
  | Not                      (* pop b, push logical negation *)

  (* Control flow *)
  | Label of string          (* marks a jump target; no-op at runtime *)
  | Jump of string           (* unconditional jump to label *)
  | BranchIfFalse of string  (* pop top; jump to label if 0 *)

  (* I/O *)
  | Print                    (* pop top of stack and print it *)
  | Pop                      (* discard top of stack (used after expr-stmts) *)

  (* Functions *)
  | Call of string * int     (* call named function; top n values are args *)
  | Return                   (* function return; top-level stores result in "return" *)

  (* Misc *)
  | Nop                      (* no operation (compiled from NoOp / skip) *)

type program = instr list

(* Saved caller frame for function call/return *)
type frame = { ret_pc : int; saved_vars : (string, int) Hashtbl.t }

(* Stack machine state *)
type state = { stack : int list; vars : (string, int) Hashtbl.t; pc : int; call_stack : frame list }

(* Scan the program for the index of a Label instruction *)
let find_label prog lbl =
  let rec aux i = function
    | [] -> failwith ("Label not found: " ^ lbl)
    | Label l :: _ when l = lbl -> i
    | _ :: rest -> aux (i + 1) rest
  in
  aux 0 prog

let step prog state =
  let instr = List.nth prog state.pc in
  let next s = { s with pc = s.pc + 1 } in
  match instr with

  | Push n ->
    next { state with stack = n :: state.stack }

  | Load x ->
    let v = Hashtbl.find state.vars x in
    next { state with stack = v :: state.stack }

  | Store x ->
    (match state.stack with
     | v :: rest ->
       Hashtbl.replace state.vars x v;
       next { state with stack = rest }
     | [] -> failwith "Store: empty stack")

  | Declare x ->
    if Hashtbl.mem state.vars x then
      failwith ("Declare: variable already declared: " ^ x);
    Hashtbl.add state.vars x 0;
    next state

  (* Arithmetic: push n1 first, then n2, so n2 is on top *)
  | Add ->
    (match state.stack with
     | n2 :: n1 :: rest -> next { state with stack = (n1 + n2) :: rest }
     | _ -> failwith "Add: insufficient operands")

  | Sub ->
    (match state.stack with
     | n2 :: n1 :: rest -> next { state with stack = (n1 - n2) :: rest }
     | _ -> failwith "Sub: insufficient operands")

  | Mul ->
    (match state.stack with
     | n2 :: n1 :: rest -> next { state with stack = (n1 * n2) :: rest }
     | _ -> failwith "Mul: insufficient operands")

  | Div ->
    (match state.stack with
     | n2 :: n1 :: rest ->
       if n2 = 0 then failwith "Div: division by zero";
       next { state with stack = (n1 / n2) :: rest }
     | _ -> failwith "Div: insufficient operands")

  | Pow ->
    (match state.stack with
     | n2 :: n1 :: rest ->
       let result = int_of_float (Float.pow (float_of_int n1) (float_of_int n2)) in
       next { state with stack = result :: rest }
     | _ -> failwith "Pow: insufficient operands")

  (* Comparisons: push 1 for true, 0 for false *)
  | Lt ->
    (match state.stack with
     | n2 :: n1 :: rest -> next { state with stack = (if n1 < n2  then 1 else 0) :: rest }
     | _ -> failwith "Lt: insufficient operands")

  | Gt ->
    (match state.stack with
     | n2 :: n1 :: rest -> next { state with stack = (if n1 > n2  then 1 else 0) :: rest }
     | _ -> failwith "Gt: insufficient operands")

  | Lte ->
    (match state.stack with
     | n2 :: n1 :: rest -> next { state with stack = (if n1 <= n2 then 1 else 0) :: rest }
     | _ -> failwith "Lte: insufficient operands")

  | Gte ->
    (match state.stack with
     | n2 :: n1 :: rest -> next { state with stack = (if n1 >= n2 then 1 else 0) :: rest }
     | _ -> failwith "Gte: insufficient operands")

  | Eq ->
    (match state.stack with
     | v2 :: v1 :: rest -> next { state with stack = (if v1 =  v2 then 1 else 0) :: rest }
     | _ -> failwith "Eq: insufficient operands")

  | Neq ->
    (match state.stack with
     | v2 :: v1 :: rest -> next { state with stack = (if v1 <> v2 then 1 else 0) :: rest }
     | _ -> failwith "Neq: insufficient operands")

  (* Boolean: booleans are 0/1 ints *)
  | And ->
    (match state.stack with
     | b2 :: b1 :: rest ->
       next { state with stack = (if b1 <> 0 && b2 <> 0 then 1 else 0) :: rest }
     | _ -> failwith "And: insufficient operands")

  | Or ->
    (match state.stack with
     | b2 :: b1 :: rest ->
       next { state with stack = (if b1 <> 0 || b2 <> 0 then 1 else 0) :: rest }
     | _ -> failwith "Or: insufficient operands")

  | Not ->
    (match state.stack with
     | b :: rest -> next { state with stack = (if b = 0 then 1 else 0) :: rest }
     | []        -> failwith "Not: empty stack")

  (* Control flow *)
  | Label _ ->
    next state

  | Jump lbl ->
    { state with pc = find_label prog lbl }

  | BranchIfFalse lbl ->
    (match state.stack with
     | b :: rest ->
       let pc' = if b = 0 then find_label prog lbl else state.pc + 1 in
       { state with stack = rest; pc = pc' }
     | [] -> failwith "BranchIfFalse: empty stack")

  (* I/O *)
  | Print ->
    (match state.stack with
     | v :: rest ->
       print_int v; print_newline ();
       next { state with stack = rest }
     | [] -> failwith "Print: empty stack")

  | Pop ->
    (match state.stack with
     | _ :: rest -> next { state with stack = rest }
     | []        -> failwith "Pop: empty stack")

  (* Functions *)
  | Call (f, _n) ->
    let frame = { ret_pc = state.pc + 1; saved_vars = state.vars } in
    let new_vars = Hashtbl.create 16 in
    let dest = find_label prog f in
    { state with vars = new_vars; pc = dest; call_stack = frame :: state.call_stack }

  | Return ->
    (match state.stack with
     | v :: rest ->
       (match state.call_stack with
        | frame :: frames ->
          (* Function return: restore caller frame, leave return value on stack *)
          { stack = v :: rest; vars = frame.saved_vars; pc = frame.ret_pc; call_stack = frames }
        | [] ->
          (* Top-level return from Main: store result and halt *)
          Hashtbl.replace state.vars "return" v;
          { state with stack = rest; pc = List.length prog })
     | [] -> failwith "Return: empty stack")

  | Nop ->
    next state

let run prog init =
  let len = List.length prog in
  let rec loop state =
    if state.pc >= len then state
    else loop (step prog state)
  in
  loop init


(***************)

let fresh_label =
  let n = ref 0 in
  fun prefix -> incr n; prefix ^ string_of_int !n

(* ------------------------------------------------------------------ *)
(*  Pretty printers                                                     *)
(* ------------------------------------------------------------------ *)

let string_of_instr = function
  | Push n            -> Printf.sprintf "Push %d" n
  | Load x            -> Printf.sprintf "Load %s" x
  | Store x           -> Printf.sprintf "Store %s" x
  | Declare x         -> Printf.sprintf "Declare %s" x
  | Add               -> "Add"
  | Sub               -> "Sub"
  | Mul               -> "Mul"
  | Div               -> "Div"
  | Pow               -> "Pow"
  | Lt                -> "Lt"
  | Gt                -> "Gt"
  | Lte               -> "Lte"
  | Gte               -> "Gte"
  | Eq                -> "Eq"
  | Neq               -> "Neq"
  | And               -> "And"
  | Or                -> "Or"
  | Not               -> "Not"
  | Label l           -> Printf.sprintf "Label %s:" l
  | Jump l            -> Printf.sprintf "Jump %s" l
  | BranchIfFalse l   -> Printf.sprintf "BranchIfFalse %s" l
  | Print             -> "Print"
  | Pop               -> "Pop"
  | Call (f, n)       -> Printf.sprintf "Call %s/%d" f n
  | Return            -> "Return"
  | Nop               -> "Nop"

(** Print the program as a numbered listing.
    The current pc (if provided) is marked with an arrow. *)
let string_of_program ?(pc = -1) prog =
  let lines = List.mapi (fun i instr ->
    let arrow = if i = pc then " ->" else "   " in
    Printf.sprintf "%s %3d  %s" arrow i (string_of_instr instr)
  ) prog in
  String.concat "\n" lines

(** Print the machine state: pc, stack (top-first), and variable bindings. *)
let string_of_state prog state =
  let stack_str =
    if state.stack = [] then "[]"
    else "[" ^ String.concat ", " (List.map string_of_int state.stack) ^ "]"
  in
  let bindings =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) state.vars []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> List.map (fun (k, v) -> Printf.sprintf "%s=%d" k v)
    |> String.concat ", "
  in
  let vars_str = if bindings = "" then "{}" else "{ " ^ bindings ^ " }" in
  let instr_str =
    match List.nth_opt prog state.pc with
    | Some i -> string_of_instr i
    | None   -> "<end of program>"
  in
  Printf.sprintf "pc=%d  (%s)\nstack: %s\nvars:  %s\ncalls: %d"
    state.pc instr_str stack_str vars_str (List.length state.call_stack)

let print_program ?(pc = -1) prog =
  print_string (string_of_program ~pc prog);
  print_newline ()

let print_state prog state =
  print_string (string_of_state prog state);
  print_newline ()

let var = Hashtbl.find 