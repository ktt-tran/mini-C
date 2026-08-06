exception InvalidInputException of string
exception TypeError of string
exception DeclareError of string
exception DivByZeroError

type data_type =
  | Int_Type
  | Bool_Type
  [@@deriving show { with_path = false }]

type expr =
  | ID of string
  | Int of int
  | Bool of bool
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Pow of  expr * expr
  | Greater of expr * expr
  | Less of expr * expr
  | GreaterEqual of expr * expr
  | LessEqual of expr * expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Or of expr * expr
  | And of expr * expr
  | Not of expr
  | Call of string * expr list              (* function call *)
  [@@deriving show { with_path = false }]

type stmt =
  (* | NoOp                                    (* For parser termination *)
  | Seq of stmt * stmt                      True sequencing instead of lists *)
  | Declare of data_type * string           (* Here the expr must be an id but students don't know polymorphic variants *)
  | Assign of string * expr                 (* Again, LHS must be an ID *)
  | If of expr * stmt list * stmt list      (* If guard is an expr, body of each block is a stmt list *)
  | For of string * expr * expr * stmt list (* String is a variable, ints represent ranges of binding, body is a stmt list *)
  | While of expr * stmt list               (* Guard is an expr, body is a stmt list *)
  | Print of expr                           (* Print the result of an expression *)
  | Return of expr                          (* Return an expression or an ID (binding) *)
  [@@deriving show { with_path = false }]

type param = string * data_type
[@@deriving show { with_path = false }]

type func_def = {
  ret_type : data_type;
  name     : string;
  params   : param list;
  body     : stmt list;
}
[@@deriving show { with_path = false }]

(* top-level definitions *)
(* fun needs to be here to prevent nested functions *)
type defn =
  | Defns of defn list                      (* multiple definitions in file, last ought to be a Main *)
  (*| Multi of defn * defn                  (* multiple definitions in file *) *) (* <-- from project 3, see readme for change *)
  | Fun   of func_def                       (* return type, name, params, body *)
  | Main  of stmt list                      (* main only has a body; should denote end of file *)
  [@@deriving show { with_path = false }]

type value = 
  | Int_Val of int
  | Bool_Val of bool
  [@@deriving show { with_path = false }]
  
(* store declarations and functions in environment *)
type environment = {
  defns: (string * func_def) list;
  bindings: (string * value) list
} [@@deriving show { with_path = false }]


let c = ref 0

let fresh () = let r = !c in c:= !c + 1; r

let reset () = c:= 0

