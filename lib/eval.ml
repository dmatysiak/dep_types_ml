open Env
open Error
open Monadic
open Printers
open Types

let rec eval env expr =
  match expr with
  | Var x -> lookup_var env x
  | Lambda (x, body) -> Ok (VClosure (env, x, body))
  | App (rator, rand) ->
    eval env rator >>= fun f -> eval env rand >>= fun arg -> do_apply f arg
  | Zero -> Ok VZero
  | Add1 n -> eval env n >>= fun n -> Ok (VAdd1 n)
  | Rec (t, tgt, base, step) ->
    eval env tgt
    >>= fun vtgt ->
    eval env base
    >>= fun vbase -> eval env step >>= fun vstep -> do_rec t vtgt vbase vstep
  | Ann (expr, _) -> eval env expr

and do_apply fn arg =
  match fn with
  | VClosure (env, x, body) -> eval (extend env x arg) body
  | VNeutral (TArr (t0, t1), neu) ->
    Ok (VNeutral (t1, NApp (neu, Normal { normal_type = t0; normal_value = arg })))
  | _ ->
    internal_error
      (String.concat ": " [ "Invalid `value` constructor for `do_apply`"; print_val fn ])

and do_rec t tgt base step =
  match tgt with
  | VZero -> Ok base
  | VAdd1 n ->
    do_apply step n >>= fun f -> do_rec t n base step >>= fun arg -> do_apply f arg
  | VNeutral (TNat, neu) ->
    let nrecval =
      NRec
        ( t
        , neu
        , Normal { normal_type = t; normal_value = base }
        , Normal { normal_type = TArr (TNat, TArr (t, t)); normal_value = step } )
    in
    Ok (VNeutral (t, nrecval))
  | _ ->
    internal_error
      (String.concat ": " [ "Invalid `value` constructor for `do_rec`"; print_val tgt ])
;;

let defs_to_ctx ds =
  env_map
    (fun d ->
      match d with
      | Normal { normal_type = nt; _ } -> nt)
    ds
;;

let defs_to_env ds =
  env_map
    (fun d ->
      match d with
      | Normal { normal_value = nv; _ } -> nv)
    ds
;;

let rec add_defs env defs =
  match defs with
  | [] -> env
  | (nm, e) :: defs ->
    let v = eval env e in
    (match v with
     | Ok v -> add_defs (extend env nm v) defs
     | _ -> env)
;;
