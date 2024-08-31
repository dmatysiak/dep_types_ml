open Env
open Error
open Monadic
open Types

let rec synth ctx expr =
  match expr with
  | Var x -> lookup_var ctx x
  | App (rator, rand) ->
    synth ctx rator
    >>= fun t ->
    (match t with
     | TArr (t_arg, t_ret) -> check ctx rand t_arg >>= fun _ -> Ok t_ret
     | bad_ty -> type_error "Not a function type" bad_ty)
  | Rec (t, tgt, base, step) ->
    synth ctx tgt
    >>= fun t_tgt ->
    (match t_tgt with
     | TNat ->
       check ctx base t_tgt
       >>= fun _ -> check ctx step (TArr (TNat, TArr (t, t))) >>= fun _ -> Ok t
     | bad_ty -> type_error "Bad type for rec" bad_ty)
  | Ann (e, t) -> check ctx e t
  | Zero -> Ok TNat
  | Add1 n -> check ctx n TNat >>= fun _ -> Ok TNat
  | bad_expr -> expr_error "Cannot synthesize type for" bad_expr

and check ctx expr t =
  match expr with
  | Lambda (x, body) ->
    (match t with
     | TArr (t_arg, t_ret) -> check (extend ctx x t_arg) body t_ret >>= fun _ -> Ok t
     | bad_ty -> type_error "Lambda requires a function type; given" bad_ty)
  | Zero ->
    (match t with
     | TNat -> Ok t
     | bad_ty -> type_error "Bad type for Nat" bad_ty)
  | Add1 n -> check ctx n t >>= fun _ -> Ok t
  | other ->
    synth ctx other
    >>= fun t_res -> if t_res == t then Ok t else type_error_mismatch t t_res
;;

let rec add_type_defs ctx defs =
  match defs with
  | [] -> ctx
  | (nm, e) :: defs ->
    let t = synth ctx e in
    (match t with
     | Ok t -> add_type_defs (extend ctx nm t) defs
     | _ -> ctx)
;;
