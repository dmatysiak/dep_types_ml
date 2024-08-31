open OUnit2
open Dep_types.Env
open Dep_types.Typechecker
open Dep_types.Types
open Test_lib

let test_synth_var _ =
  let expected = Ok TNat
  and ctx = extend init_ctx (Name "foo") TNat in
  let actual = synth ctx (Var (Name "foo")) in
  assert_equal_ty expected actual
;;

let test_synth_app _ =
  let expected = Ok (TArr (TNat, TNat))
  and ctx = extend init_ctx (Name "foo") (TArr (TArr (TNat, TNat), TArr (TNat, TNat))) in
  let actual = synth ctx (App (Var (Name "foo"), Lambda (Name "y", Var (Name "y")))) in
  assert_equal_ty expected actual
;;

let test_synth_ann _ =
  let expected = Ok (TArr (TNat, TNat)) in
  let actual =
    synth init_ctx (Ann (Lambda (Name "y", Var (Name "y")), TArr (TNat, TNat)))
  in
  assert_equal_ty expected actual
;;

let test_synth_rec_zero _ =
  let expected = Ok TNat in
  let actual =
    synth
      init_ctx
      (Rec (TNat, Zero, Zero, Lambda (Name "x", Lambda (Name "y", Add1 (Var (Name "y"))))))
  in
  assert_equal_ty expected actual
;;

let test_synth_rec_one _ =
  let expected = Ok TNat in
  let actual =
    synth
      init_ctx
      (Rec
         ( TNat
         , Add1 Zero
         , Add1 Zero
         , Lambda (Name "x", Lambda (Name "y", Add1 (Var (Name "y")))) ))
  in
  assert_equal_ty expected actual
;;

let test_synth_zero _ =
  let expected = Ok TNat in
  let actual = synth init_ctx Zero in
  assert_equal_ty expected actual
;;

let test_synth_one _ =
  let expected = Ok TNat in
  let actual = synth init_ctx (Add1 Zero) in
  assert_equal_ty expected actual
;;

let test_check_lambda _ =
  let expected = Ok (TArr (TNat, TNat)) in
  let actual =
    check init_ctx (Lambda (Name "y", Add1 (Var (Name "y")))) (TArr (TNat, TNat))
  in
  assert_equal_ty expected actual
;;

let test_check_zero _ =
  let expected = Ok TNat in
  let actual = check init_ctx Zero TNat in
  assert_equal_ty expected actual
;;

let test_check_one _ =
  let expected = Ok TNat in
  let actual = check init_ctx (Add1 Zero) TNat in
  assert_equal_ty expected actual
;;

let test_check_rec _ =
  let expected = Ok TNat in
  let actual =
    check
      init_ctx
      (Rec
         ( TNat
         , Add1 Zero
         , Add1 Zero
         , Lambda (Name "x", Lambda (Name "y", Add1 (Var (Name "y")))) ))
      TNat
  in
  assert_equal_ty expected actual
;;

let test_check_var _ =
  let expected = Ok TNat
  and ctx = extend init_ctx (Name "foo") TNat in
  let actual = check ctx (Var (Name "foo")) TNat in
  assert_equal_ty expected actual
;;

let test_check_app _ =
  let expected = Ok (TArr (TNat, TNat))
  and ctx = extend init_ctx (Name "foo") (TArr (TArr (TNat, TNat), TArr (TNat, TNat))) in
  let actual =
    check
      ctx
      (App (Var (Name "foo"), Lambda (Name "y", Var (Name "y"))))
      (TArr (TNat, TNat))
  in
  assert_equal_ty expected actual
;;

let test_check_ann _ =
  let expected = Ok (TArr (TNat, TNat)) in
  let actual =
    check
      init_ctx
      (Ann (Lambda (Name "y", Var (Name "y")), TArr (TNat, TNat)))
      (TArr (TNat, TNat))
  in
  assert_equal_ty expected actual
;;

let suite =
  "Typechecker tests"
  >::: [ "Synthesize Var" >:: test_synth_var
       ; "Synthesize App" >:: test_synth_app
       ; "Synthesize Annotation" >:: test_synth_ann
       ; "Synthesize Recursion (base=Zero)" >:: test_synth_rec_zero
       ; "Synthesize Recursion (base=One)" >:: test_synth_rec_one
       ; "Synthesize Zero" >:: test_synth_zero
       ; "Synthesize One" >:: test_synth_one
       ; "Check Lambda" >:: test_check_lambda
       ; "Check Zero" >:: test_check_zero
       ; "Check One" >:: test_check_one
       ; "Check Rec" >:: test_check_rec
       ; "Check Var" >:: test_check_var
       ; "Check App" >:: test_check_app
       ; "Check Annotation" >:: test_check_ann
       ]
;;

let () = run_test_tt_main suite
