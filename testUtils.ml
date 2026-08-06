open P4
open SmallCTypes
open Eval
open Stackmachine_utils
open Stackmachine

module A = Alcotest

let value_to_int = function
  | Int_Val n  -> n
  | Bool_Val b -> if b then 1 else 0

(** Compile [defn], run it on the stack machine, then also run it
    through [eval], and assert both agree on the [return] binding. *)
let assert_sm_matches_eval defn expected_eval_output =
  (* eval path *)
  let eval_env = (
    match expected_eval_output with
    |None -> (
      match eval_or_none { defns = []; bindings = [] } defn with
      |None -> failwith "test case issue, no eval provided *and* no expected_eval_output provided"
      |Some x -> x
    )
    |Some x -> x
  ) in
  let expected = value_to_int (List.assoc "return" eval_env.bindings) in
  (* stack machine path *)
  let prog  = compile defn in
  let init  = { stack = []; vars = Hashtbl.create 16; pc = 0; call_stack = [] } in
  let final = run prog init in
  let actual = Hashtbl.find final.vars "return" in
  A.(check int) "return value matches eval" expected actual

(* Wrap a statement list in a Main so it is a valid defn *)
let main s = Main s

(***********************************)

open SmallCTypes
open Typecheck

type run_test_mode = Typecheck

(* toplevel expressions are statements *)
(* we can use normal structural equality for 
   this and ppx_deriving gives us a pretty printer! *)
let ast = A.testable pp_stmt (=)

let parse_c_expr s =
  let lexbuf = Lexing.from_string s in
  match Parser.prog Lexer.token lexbuf with
  | Main body -> body
  | Fun { body; _ } -> body
  | _ -> failwith "parse_c_expr: expected a single function definition"

let type_error_handler erroneous =
  (* checks that result of thunk passed in (erroneous) produces any typeError *)
  A.match_raises
    ""
    (fun e -> match e with TypeError(_) -> true | _ -> false)
    erroneous

let declare_error_handler erroneous =
  (* checks that result of thunk passed in (erroneous) produces any declareError *)
  A.match_raises
    ""
    (fun e -> match e with DeclareError(_) -> true | _ -> false)
    erroneous

type test_input = {untyped:stmt list;typed:stmt list;should_typecheck:bool}

let run_test ~mode {typed=typed; should_typecheck=our_type; untyped=untyped} _ =
  match mode with
  | Typecheck ->
      if our_type then
      let student = typed |> typecheck in
      A.(check bool)
        ("Type error:\nexpected: " 
          ^ if our_type then "true got error" else "error got true")
        our_type
        student
      else
        type_error_handler (fun () -> typed |> typecheck |> ignore)