open OUnit2
open Dep_types.Env
open Dep_types.Eval
open Dep_types.Run
open Dep_types.Types
open Test_lib

let basic_defs =
  [ Name "foo", Lambda (Name "x", Var (Name "x"))
  ; Name "bar", Lambda (Name "y", Var (Name "y"))
  ; Name "qux", Lambda (Name "z", Var (Name "z"))
  ]
;;

let basic_env = add_defs init_env basic_defs

let test_run_program_lambda _ =
  let expected = Ok (Lambda (Name "x", Var (Name "x")))
  and actual = run_program [] (TArr (TNat, TNat)) (Lambda (Name "x", Var (Name "x"))) in
  assert_equal_expr expected actual
;;

let test_run_program_no_such_var _ =
  let expected = Error (Message "Not found: foo")
  and actual = run_program [] TNat (Var (Name "foo")) in
  assert_equal expected actual
;;

let test_run_program_zero _ =
  let expected = Ok Zero
  and actual = run_program basic_defs TNat Zero in
  assert_equal_expr expected actual
;;

let test_run_program_two _ =
  let expected = Ok (Add1 (Add1 Zero))
  and actual = run_program basic_defs TNat (Add1 (Add1 Zero)) in
  assert_equal_expr expected actual
;;

let test_run_program_var _ =
  let expected = Ok (Lambda (Name "x", Var (Name "x")))
  and actual = run_program basic_defs (TArr (TNat, TNat)) (Var (Name "foo")) in
  assert_equal_expr expected actual
;;

let test_run_program_app_zero _ =
  let expected = Ok Zero
  and actual = run_program basic_defs TNat (App (Var (Name "foo"), Zero)) in
  assert_equal_expr expected actual
;;

let test_run_program_app _ =
  let expected = Ok (Lambda (Name "y", Var (Name "y")))
  and actual =
    run_program basic_defs (TArr (TNat, TNat)) (App (Var (Name "foo"), Var (Name "bar")))
  in
  assert_equal_expr expected actual
;;

let suite =
  "Run tests"
  >::: [ "Lambda" >:: test_run_program_lambda
       ; "Missing var" >:: test_run_program_no_such_var
       ; "Zero" >:: test_run_program_zero
       ; "Two" >:: test_run_program_two
       ; "Var" >:: test_run_program_var
       ; "Application of closures" >:: test_run_program_app
       ; "Application with Nat (Zero)" >:: test_run_program_app_zero
       ]
;;

let () = run_test_tt_main suite
