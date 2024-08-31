open OUnit2
open Dep_types.Readback
open Dep_types.Types
open Test_lib

let test_readback_zero _ =
  let expected = Ok Zero
  and actual = Ok Zero in
  assert_equal_expr expected actual
;;

let test_readback_two _ =
  let expected = Ok (Add1 (Add1 Zero))
  and actual = readback [] TNat (VAdd1 (VAdd1 VZero)) in
  assert_equal_expr expected actual
;;

let test_readback_lambda _ =
  let expected = Ok (Lambda (Name "x", Var (Name "x")))
  and actual =
    readback [] (TArr (TNat, TNat)) (VClosure (Env [], Name "x", Var (Name "x")))
  in
  assert_equal_expr expected actual
;;

let suite =
  "Readback tests"
  >::: [ "Zero" >:: test_readback_zero
       ; "Two" >:: test_readback_two
       ; "Lambda" >:: test_readback_lambda
       ]
;;

let () = run_test_tt_main suite
