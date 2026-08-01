(* The first section of the grammar definition, called the *header*,
   is the part that appears below between %{ and %}.  It is code
   that will simply be copied literally into the generated parser.ml. 
   Here we use it just to open the Ast module so that, later on
   in the grammar definition, we can write expressions like 
   [Int i] instead of [Ast.Int i]. *)

%{
open SmallCTypes
%}

(* The next section of the grammar definition, called the *declarations*,
   first declares all the lexical *tokens* of the language.  These are 
   all the kinds of tokens we can expect to read from the token stream.
   Note that each of these is just a descriptive name---nothing so far
   says that LPAREN really corresponds to '(', for example.  The tokens
   that have a <type> annotation appearing in them are declaring that
   they will carry some additional data along with them.  In the
   case of INT, that's an OCaml int.  In the case of ID, that's
   an OCaml string. *)
%token AND
%token DIV
%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token ELSE
%token LBRACE
%token GE
%token GT
%token IF
%token ASSIGNS
%token PRINT
%token WHILE
%token PLUS
%token MINUS
%token MULT
%token NOT
%token NE
%token LPAREN
%token LE
%token LT
%token OR
%token RPAREN
%token RBRACE
%token SEMI
%token TO
%token EQUALS
%token FROM
%token MAIN
%token FOR 
%token EOF

%token INTTYPE
%token BOOLTYPE
%token COMMA
%token RETURN
%token POW

(* After declaring the tokens, we have to provide some additional information
   about precedence and associativity.  The following declarations say that
   PLUS is left associative, that IN is not associative, and that PLUS
   has higher precedence than IN (because PLUS appears on a line after IN).

   Because PLUS is left associative, "1+2+3" will parse as "(1+2)+3"
   and not as "1+(2+3)".

   Because PLUS has higher precedence than IN, "let x=1 in x+2" will
   parse as "let x=1 in (x+2)" and not as "(let x=1 in x)+2". *)

%right OR                               /* Or */
%right AND                              /* And */
%left EQUALS,NE, GT,GE,LE,LT            /* = */
%left PLUS MINUS                        /* + - * */
%left MULT  DIV                         /* *  / */
%right POW                              /* ^ */
%right NOT                              /* !e */


(* After declaring associativity and precedence, we need to declare what
   the starting point is for parsing the language.  The following
   declaration says to start with a rule (defined below) named [prog].
   The declaration also says that parsing a [prog] will return an OCaml
   value of type [SmallCTypes.stmt]. *)

%start <SmallCTypes.defn> prog
%start <SmallCTypes.expr> parse_expr
%start <SmallCTypes.stmt list> parse_stmt

(* The following %% ends the declarations section of the grammar definition. *)

%%

(* Now begins the *rules* section of the grammar definition.  This is a list
   of rules that are essentially in Backus-Naur Form (BNF), although where in 
   BNF we would write "::=" these rules simply write ":".
   
   The format of a rule is
   
     name:
       | production { action }
       | production { action }
       | etc.
       ;
       
    The *production* is the sequence of *symbols* that the rule matches. 
    A symbol is either a token or the name of another rule. 
    The *action* is the OCaml value to return if a match occurs. 
    Each production can *bind* the value carried by a symbol and use
    that value in its action.  This is perhaps best understood by example... *)
   
prog:
  | ds = defn_list; EOF { ds }
  ;

parse_expr:
  | e = expr; EOF { e }
  ;

parse_stmt:
  | s = stmt; EOF { s }
  ;

defn_list:
  | d = single_defn { d }
  | d = single_defn; ds = more_defns { Defns(d :: ds) }
  ;

more_defns:
  | d = single_defn { [d] }
  | d = single_defn; ds = more_defns { d :: ds }
  ;

single_defn:
  | INTTYPE; MAIN; LPAREN; RPAREN; LBRACE; s = stmt; RBRACE { Main(s) }
  | INTTYPE; MAIN; LPAREN; RPAREN; LBRACE; RBRACE { Main([]) }
  | rt = ret_type; x = IDENT; LPAREN; ps = param_list; RPAREN; LBRACE; s = stmt; RBRACE { Fun { ret_type=rt; name=x; params=ps; body=s } }
  | rt = ret_type; x = IDENT; LPAREN; ps = param_list; RPAREN; LBRACE; RBRACE { Fun { ret_type=rt; name=x; params=ps; body=[] } }
  ;

ret_type:
  | INTTYPE { Int_Type }
  | BOOLTYPE { Bool_Type }
  ;

param_list:
  | { [] }
  | p = param { [p] }
  | p = param; COMMA; ps = param_list { p :: ps }
  ;

param:
  | INTTYPE; x = IDENT { (x, Int_Type) }
  | BOOLTYPE; x = IDENT { (x, Bool_Type) }
  ;
	
stmt:
  | { [] }
  | s = stmtopt; rest = stmt { s :: rest }
;

stmtopt:
  | INTTYPE; x = IDENT; SEMI { Declare(Int_Type, x) }
  | BOOLTYPE; x = IDENT; SEMI { Declare(Bool_Type, x) }
  | x = IDENT; ASSIGNS; e = expr; SEMI { Assign(x,e) }
  | IF; LPAREN; e = expr; RPAREN; LBRACE; s1 = stmt; RBRACE; { If(e,s1,[]) }
  | IF; LPAREN; e = expr; RPAREN; LBRACE; s1 = stmt; RBRACE; ELSE; LBRACE; s2 = stmt; RBRACE; { If(e,s1,s2) }
  | FOR; LPAREN; x = IDENT; FROM; e1 = expr; TO; e2 = expr; RPAREN; LBRACE; s = stmt; RBRACE { For(x,e1,e2,s) }
  | WHILE; LPAREN; e = expr; RPAREN; LBRACE; s = stmt; RBRACE { While(e,s) }
  | PRINT; LPAREN; e = expr; RPAREN; SEMI { Print(e) }
  | RETURN; e = expr; SEMI { Return(e) }

expr:
  | e = simple_expr { e }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MULT; e2 = expr { Mult(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | e1 = expr; POW; e2 = expr { Pow(e1,e2) }
  | e1 = expr; AND; e2 = expr { And(e1,e2) }
  | e1 = expr; OR; e2 = expr { Or(e1,e2) }
  | e1 = expr; NE; e2 = expr { NotEqual(e1,e2) }
  | e1 = expr; EQUALS; e2 = expr { Equal(e1,e2) }
  | e1 = expr; GT; e2 = expr { Greater(e1,e2) }
  | e1 = expr; GE; e2 = expr { GreaterEqual(e1,e2) }
  | e1 = expr; LT; e2 = expr { Less(e1,e2) }
  | e1 = expr; LE; e2 = expr { LessEqual(e1,e2) }
  | NOT; e = expr { Not(e) }
  | LPAREN; e = expr; RPAREN { e }
;

simple_expr:
  | i = INT { Int i }
  | b = BOOL { Bool b }
  | x = IDENT; LPAREN; args = arg_list; RPAREN { Call(x, args) }
  | x = IDENT { ID x }
;

arg_list:
  | { [] }
  | e = expr { [e] }
  | e = expr; COMMA; es = arg_list { e :: es }
;
(* And that's the end of the grammar definition. *)
