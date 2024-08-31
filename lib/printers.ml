open Monadic
open Types

module type Printer = sig
  type t

  val print : t -> string
end

let format_term constr args =
  String.concat "" [ constr; "("; String.concat ", " args; ")" ]
;;

let print_name nm =
  match nm with
  | Name n -> n
;;

let print_msg m =
  match m with
  | Message m -> m
;;

let rec print_ty t =
  match t with
  | TNat -> "Nat"
  | TArr (t0, t1) -> String.concat " " [ print_ty t0; "->"; print_ty t1 ]

and print_neutral n =
  match n with
  | NVar nm -> print_name nm
  | NApp (neu, v) -> format_term "NApp" [ print_neutral neu; print_normal v ]
  | NRec (t, neu, no0, no1) ->
    format_term
      "NRec"
      [ print_ty t; print_neutral neu; print_normal no0; print_normal no1 ]

and print_val v =
  match v with
  | VClosure (env, name, expr) ->
    format_term "VClosure" [ print_env env; print_name name; print_expr expr ]
  | VNeutral (t, neu) -> format_term "VNeutral" [ print_ty t; print_neutral neu ]
  | VZero -> "VZero"
  | VAdd1 v -> format_term "VAdd1" [ print_val v ]

and print_expr e =
  match e with
  | Var nm -> format_term "Var" [ print_name nm ]
  | Lambda (nm, expr) -> format_term "Lambda" [ print_name nm; print_expr expr ]
  | App (e0, e1) -> format_term "App" [ print_expr e0; print_expr e1 ]
  | Zero -> "0"
  | Add1 expr -> String.concat "" [ "Add1("; print_expr expr; ")" ]
  | Rec (t, e0, e1, e2) ->
    format_term "Rec" [ print_ty t; print_expr e0; print_expr e1; print_expr e2 ]
  | Ann (expr, t) -> format_term "Ann" [ print_expr expr; print_ty t ]

and print_normal n =
  match n with
  | Normal { normal_type = nt; normal_value = nv } ->
    format_term "Normal" [ print_ty nt; print_val nv ]

and print_env_f f e =
  String.concat
    "; "
    (match e with
     | Env bs ->
       List.map
         (fun b ->
           match b with
           | Binding (n, x) -> String.concat " = " [ print_name n; f x ])
         bs)

and print_env e =
  (match e with
   | Env bs ->
     List.map
       (fun b ->
         match b with
         | Binding (n, v) -> String.concat " = " [ print_name n; print_val v ])
       bs)
  |> String.concat "; "
  |> Printf.sprintf "{%s}"

and print_ctx c =
  (match c with
   | Env bs ->
     List.map
       (fun b ->
         match b with
         | Binding (n, c) -> String.concat " = " [ print_name n; print_ty c ])
       bs)
  |> String.concat "; "
  |> Printf.sprintf "{%s}"
;;

module NamePrinter = struct
  type t = name

  let print = print_name
end

module MessagePrinter = struct
  type t = message

  let print = print_msg
end

module TypePrinter = struct
  type t = ty

  let print = print_ty
end

module NeutralPrinter = struct
  type t = neutral

  let print = print_neutral
end

module ValuePrinter = struct
  type t = value

  let print = print_val
end

module ExprPrinter = struct
  type t = expr

  let print = print_expr
end

module NormalPrinter = struct
  type t = normal

  let print = print_normal
end

module EnvPrinter = struct
  type t = value env

  let print = print_env
end

module CtxPrinter = struct
  type t = ty env

  let print = print_ctx
end

let print_result_with printer x =
  let msg = x >>= fun v -> Ok (printer v) in
  match msg with
  | Ok m -> m
  | Error m ->
    (match m with
     | Message s -> s)
;;
