open Types

val eval : value env -> expr -> (value, message) result
val do_apply : value -> value -> (value, message) result
val do_rec : ty -> value -> value -> value -> (value, message) result
val defs_to_ctx : normal env -> ty binding list
val defs_to_env : normal env -> value binding list
val add_defs : value env -> (name * expr) list -> value env
