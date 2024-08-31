open OUnit2
open Dep_types.Env
open Dep_types.Types

let test_extend_empty _ = assert_equal init_env init_env

let test_extend_add_to_empty _ =
  let expected = Env [ Binding (Name "foo", VClosure (Env [], Name "x", Var (Name "x"))) ]
  and actual =
    extend init_env (Name "foo") (VClosure (init_env, Name "x", Var (Name "x")))
  in
  assert_equal expected actual
;;

let test_extend_add_to_one _ =
  let expected =
    Env
      [ Binding (Name "bar", VClosure (Env [], Name "y", Var (Name "y")))
      ; Binding (Name "foo", VClosure (Env [], Name "x", Var (Name "x")))
      ]
  and intermediate =
    extend init_env (Name "foo") (VClosure (init_env, Name "x", Var (Name "x")))
  in
  let actual =
    extend intermediate (Name "bar") (VClosure (init_env, Name "y", Var (Name "y")))
  in
  assert_equal expected actual
;;

let test_lookup_var_not_found _ =
  let expected = Error (Message "Not found: foo")
  and actual = lookup_var init_env (Name "foo") in
  assert_equal expected actual
;;

let test_lookup_var_found _ =
  let expected = Ok (VClosure (init_env, Name "x", Var (Name "x")))
  and initial_env =
    extend init_env (Name "bar") (VClosure (init_env, Name "x", Var (Name "x")))
  in
  let actual = lookup_var initial_env (Name "bar") in
  assert_equal expected actual
;;

let suite =
  "Env tests"
  >::: [ "Identity of initial environment" >:: test_extend_empty
       ; "Extend empty environment" >:: test_extend_add_to_empty
       ; "Extend non-empty environment" >:: test_extend_add_to_one
       ; "Look-up missing var" >:: test_lookup_var_not_found
       ; "Look-up var" >:: test_lookup_var_found
       ]
;;

let () = run_test_tt_main suite
