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

(* Execute one instruction; returns the next state *)
val step : program -> state -> state

(* Run a program to completion from an initial state *)
val run : program -> state -> state

(* Pretty printers *)
val string_of_instr   : instr -> string

(** [string_of_program ~pc prog] renders the program as a numbered listing.
    If [pc] is supplied, that line is marked with an arrow. *)
val string_of_program : ?pc:int -> program -> string

(** [string_of_state prog state] renders pc, current instruction, stack, and vars. *)
val string_of_state   : program -> state -> string

val print_program : ?pc:int -> program -> unit
val print_state   : program -> state -> unit

val var: (string, int) Hashtbl.t -> string -> int

val fresh_label : string -> string