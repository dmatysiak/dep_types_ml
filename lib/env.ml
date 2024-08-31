open Error
open Types

let init_env : value env = Env []
let init_ctx : ty env = Env []
let init_defs : normal env = Env []

let env_map f env =
  match env with
  | Env entries ->
    List.map
      (fun e ->
        match e with
        | Binding (nm, v) -> Binding (nm, f v))
      entries
;;

let rec lookup_var env nm =
  match env with
  | Env [] -> failure_not_found nm
  | Env (Binding (enm, v) :: env_rest) ->
    if nm = enm then Ok v else lookup_var (Env env_rest) nm
;;

let extend env nm v =
  let (Env e) = env in
  Env (Binding (nm, v) :: e)
;;
