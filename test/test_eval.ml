open OUnit2
open Dep_types.Env
open Dep_types.Eval
open Dep_types.Printers
open Dep_types.Types
open Test_lib

let test_eval_var_not_found _ =
  let expected = Error (Message "Not found: foo")
  and initial_env =
    extend init_env (Name "bar") (VClosure (init_env, Name "x", Var (Name "x")))
  in
  let actual = eval initial_env (Var (Name "foo")) in
  assert_equal_value expected actual
;;

let test_eval_var _ =
  let expected = Ok (VClosure (init_env, Name "x", Var (Name "x")))
  and initial_env =
    extend init_env (Name "bar") (VClosure (init_env, Name "x", Var (Name "x")))
  in
  let actual = eval initial_env (Var (Name "bar")) in
  assert_equal_value expected actual
;;

let test_eval_zero _ =
  let expected = Ok VZero
  and actual = eval init_env Zero in
  assert_equal_value expected actual
;;

let test_eval_two _ =
  let expected = Ok (VAdd1 (VAdd1 VZero))
  and actual = eval init_env (Add1 (Add1 Zero)) in
  assert_equal_value expected actual
;;

let test_eval_lambda _ =
  let expected = Ok (VClosure (init_env, Name "x", Var (Name "x")))
  and actual = eval init_env (Lambda (Name "x", Var (Name "x"))) in
  assert_equal_value expected actual
;;

let test_eval_app_val _ =
  let expected = Ok (VClosure (init_env, Name "y", Var (Name "y")))
  and actual =
    eval
      init_env
      (App (Lambda (Name "x", Var (Name "x")), Lambda (Name "y", Var (Name "y"))))
  in
  assert_equal_value expected actual
;;

let test_eval_app_var_1 _ =
  let expected =
    Ok
      (VClosure
         ( Env [ Binding (Name "foo", VClosure (init_env, Name "x", Var (Name "x"))) ]
         , Name "y"
         , Var (Name "y") ))
  and initial_env =
    extend init_env (Name "foo") (VClosure (init_env, Name "x", Var (Name "x")))
  in
  let actual =
    eval initial_env (App (Var (Name "foo"), Lambda (Name "y", Var (Name "y"))))
  in
  assert_equal_value expected actual
;;

let test_eval_app_var_2 _ =
  let expected = Ok (VClosure (init_env, Name "x", Var (Name "x")))
  and initial_env =
    extend init_env (Name "bar") (VClosure (init_env, Name "x", Var (Name "x")))
  in
  let actual =
    eval initial_env (App (Lambda (Name "y", Var (Name "y")), Var (Name "bar")))
  in
  assert_equal_value expected actual
;;

let test_eval_app_var_3 _ =
  let expected = Ok (VClosure (init_env, Name "y", Var (Name "y")))
  and initial_env =
    extend
      (extend init_env (Name "foo") (VClosure (init_env, Name "x", Var (Name "x"))))
      (Name "bar")
      (VClosure (init_env, Name "y", Var (Name "y")))
  in
  let actual = eval initial_env (App (Var (Name "foo"), Var (Name "bar"))) in
  assert_equal_value expected actual
;;

let test_add_defs_empty_list _ =
  let expected = init_env
  and actual = add_defs init_env [] in
  assert_equal ~printer:EnvPrinter.print expected actual
;;

let test_add_defs_add_to_init_env _ =
  let expected =
    extend init_env (Name "foo") (VClosure (init_env, Name "y", Var (Name "y")))
  and actual = add_defs init_env [ Name "foo", Lambda (Name "y", Var (Name "y")) ] in
  assert_equal ~printer:EnvPrinter.print expected actual
;;

let test_add_defs_add_to_populated_env _ =
  let foo_env = init_env in
  let bar_env =
    extend foo_env (Name "foo") (VClosure (foo_env, Name "x", Var (Name "x")))
  in
  let qux_env =
    extend bar_env (Name "bar") (VClosure (bar_env, Name "y", Var (Name "y")))
  in
  let expected =
    Env
      [ Binding (Name "qux", VClosure (qux_env, Name "z", Var (Name "z")))
      ; Binding (Name "bar", VClosure (bar_env, Name "y", Var (Name "y")))
      ; Binding (Name "foo", VClosure (foo_env, Name "x", Var (Name "x")))
      ]
  and intermediate =
    extend init_env (Name "foo") (VClosure (init_env, Name "x", Var (Name "x")))
  in
  let actual =
    add_defs
      intermediate
      [ Name "bar", Lambda (Name "y", Var (Name "y"))
      ; Name "qux", Lambda (Name "z", Var (Name "z"))
      ]
  in
  assert_equal ~printer:EnvPrinter.print expected actual
;;

let test_rec_base_zero _ =
  let expected = Ok VZero
  and actual =
    eval init_env (Rec (TNat, Zero, Zero, Lambda (Name "y", Add1 (Var (Name "y")))))
  in
  assert_equal_value expected actual
;;

let test_rec_recur _ =
  let expected = Ok (VAdd1 (VAdd1 VZero))
  and actual =
    eval
      init_env
      (Rec
         ( TNat
         , Add1 Zero
         , Add1 Zero
         , Lambda (Name "x", Lambda (Name "y", Add1 (Var (Name "y")))) ))
  in
  assert_equal_value expected actual
;;

let suite =
  "Eval tests"
  >::: [ "Missing var" >:: test_eval_var_not_found
       ; "Var" >:: test_eval_var
       ; "Zero" >:: test_eval_zero
       ; "Two" >:: test_eval_two
       ; "Lambda" >:: test_eval_lambda
       ; "Application" >:: test_eval_app_val
       ; "Application with var in function position" >:: test_eval_app_var_1
       ; "Application with var in argument position" >:: test_eval_app_var_2
       ; "Application with vars" >:: test_eval_app_var_3
       ; "Add no defs to environment" >:: test_add_defs_empty_list
       ; "Add defs to empty environment" >:: test_add_defs_add_to_init_env
       ; "Add defs to populated environment" >:: test_add_defs_add_to_populated_env
       ; "Primitive recursion on base case Zero" >:: test_rec_base_zero
       ; "Primitive recursion on recursive case" >:: test_rec_recur
       ]
;;

let () = run_test_tt_main suite
