open OUnit2
open Dep_types.Church
open Dep_types.Types

let test_to_church_zero _ =
  let expected = Var (Name "zero")
  and actual = to_church 0 in
  assert_equal expected actual
;;

let test_to_church_one _ =
  let expected = App (Var (Name "add1"), Var (Name "zero"))
  and actual = to_church 1 in
  assert_equal expected actual
;;

let suite =
  "Church numeral tests"
  >::: [ "Convert zero to Church numeral" >:: test_to_church_zero
       ; "Convert one to Church numeral" >:: test_to_church_one
       ]
;;

let () = run_test_tt_main suite
