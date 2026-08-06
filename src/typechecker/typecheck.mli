open MiniCTypes

type var_env = (string * data_type) list
type fun_env = (string * (data_type * (string * data_type) list)) list

val typecheck_expr : fun_env -> var_env -> expr -> data_type
val typecheck_stmt : fun_env -> var_env -> stmt -> var_env
val typecheck_defn : defn -> bool
val typecheck : stmt list -> bool
