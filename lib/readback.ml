open Error
open Eval
open Monadic
open Printers
open Types

let next_name nm =
  match nm with
  | Name nm -> Name (String.concat "" [ nm; "'" ])
;;

let rec freshen used x = if List.mem x used then freshen used (next_name x) else x

and readback used t v =
  match t, v with
  | TNat, VZero -> Ok Zero
  | TNat, VAdd1 n -> readback used t n >>= fun r -> Ok (Add1 r)
  | TArr (t0, t1), VClosure (_, x, _) ->
    let x_unused = freshen used x in
    let arg = VNeutral (t0, NVar x_unused) in
    let body_expr = do_apply v arg >>= fun v -> readback used t1 v in
    (match body_expr with
     | Ok e -> Ok (Lambda (x_unused, e))
     | Error e -> internal_error (print_msg e))
  | _t0, VNeutral (_t1, neu) -> readback_neutral used neu
  | _ -> not_implemented (String.concat " : " [ print_ty t; print_val v ])

and readback_neutral used v =
  match v with
  | NVar x -> Ok (Var x)
  | NApp (rator, arg) ->
    readback_neutral used rator
    >>= fun r -> readback_normal used arg >>= fun a -> Ok (App (r, a))
  | NRec (t0, neu, base, step) ->
    readback_neutral used neu
    >>= fun neu ->
    readback_normal used base
    >>= fun base ->
    readback_normal used step >>= fun step -> Ok (Rec (t0, neu, base, step))

and readback_normal used v =
  match v with
  | Normal { normal_type = t; normal_value = v } -> readback used t v
;;
