open Types

let alpha_equiv expr0 expr1 =
  expr0 == expr1

let alpha_equiv_helper nbindings var_to_depth0 expr0 var_to_depth1 expr1 =
  expr0 == expr1
