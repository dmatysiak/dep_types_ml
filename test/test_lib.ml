open OUnit2
open Dep_types.Printers

let assert_equal_expr = assert_equal ~printer:(print_result_with ExprPrinter.print)
let assert_equal_value = assert_equal ~printer:(print_result_with ValuePrinter.print)
let assert_equal_ty = assert_equal ~printer:(print_result_with TypePrinter.print)
