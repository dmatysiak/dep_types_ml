open Env
open Eval
open Monadic
open Readback

let run_program defs ty expr =
  let env = add_defs init_env defs in
  eval env expr >>= fun v -> readback (List.map (fun (nm, _) -> nm) defs) ty v
;;
