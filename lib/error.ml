open Printers
open Types

let failure msg = Error (Message msg)

let expr_error msg e =
  let e_str = print_expr e in
  Error (Message (String.concat ": " [ msg; e_str ]))
;;

let type_error msg t =
  let t_str = print_ty t in
  Error (Message (String.concat ": " [ msg; t_str ]))
;;

let type_error_mismatch t0 t1 =
  let t0_str = print_ty t0
  and t1_str = print_ty t1 in
  Error (Message (String.concat " " [ "Expected type"; t0_str; ","; "found"; t1_str ]))
;;

let internal_error msg = failure (String.concat ": " [ "Internal error"; msg ])

let failure_not_found = function
  | Name nm -> failure (String.concat " " [ "Not found:"; nm ])
;;

let not_implemented nm = failure (String.concat ": " [ "Not implemented"; nm ])
