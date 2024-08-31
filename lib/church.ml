open Types

let church_defs =
  [ Name "zero", Lambda (Name "f", Lambda (Name "x", Var (Name "x")))
  ; ( Name "add1"
    , Lambda
        ( Name "n"
        , Lambda
            ( Name "f"
            , Lambda (Name "x", App (Var (Name "f"), App (Var (Name "n"), Var (Name "f"))))
            ) ) )
  ; ( Name "+"
    , Lambda
        ( Name "j"
        , Lambda
            ( Name "k"
            , Lambda
                ( Name "f"
                , Lambda
                    ( Name "x"
                    , App
                        ( App (Var (Name "j"), Var (Name "f"))
                        , App (App (Var (Name "k"), Var (Name "f")), Var (Name "x")) ) )
                ) ) ) )
  ]
;;

let rec to_church n =
  if n <= 0 then Var (Name "zero") else App (Var (Name "add1"), to_church (n - 1))
;;
