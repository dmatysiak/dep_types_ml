type name = Name of string
type message = Message of string

type ty =
  | TNat
  | TArr of ty * ty

type 'a binding = Binding of name * 'a
and 'a env = Env of 'a binding list

and value =
  | VClosure of value env * name * expr
  | VNeutral of ty * neutral
  | VZero
  | VAdd1 of value

  | VPi of ty * closure
  | VLambda of closure
  | VSigma of ty * closure
  | VPair of value * value
  | VNat
  | VEq of ty * value * value
  | VSame
  | VTrivial
  | VSole
  | VAbsurd
  | VAtom
  | VTick of string
  | VUniverse

and closure =
  | Closure of
      { closure_env : value env
      ; closure_name : name
      ; closure_body : expr
      }

and neutral =
  | NVar of name
  | NApp of neutral * normal
  | NRec of ty * neutral * normal * normal

and normal =
  | Normal of
      { normal_type : ty
      ; normal_value : value
      }

and expr =
  | Var of name
  | Lambda of name * expr
  | App of expr * expr
  | Zero
  | Add1 of expr
  | Rec of ty * expr * expr * expr
  | Ann of expr * ty

  | Pi of name * expr * expr
  | Sigma of name * expr *expr
  | Cons of expr * expr
  | Car of expr
  | Cdr of expr
  | Nat
  | IndNat of expr * expr * expr * expr
  | Equal of expr * expr * expr
  | Same
  | Replace of expr * expr * expr
  | Trivial
  | Sole
  | Absurd
  | IndAbsurd of expr * expr
  | Atom
  | Tick of string
  | Universe
  | The of expr * expr
