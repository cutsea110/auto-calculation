module Aop01 where

import Calculations (Calculation)
import Simplify (prove, simplify)

append_assoc_nil :: Calculation
append_assoc_nil = prove customLaws "append x (append y nil) = append (append x y) nil"
  where
    customLaws = [ "++ の最初の定義式: append x nil = x"
                 , "++ の2番目の定義式: append x (snoc y a) = snoc (append x y) a"
                 ]

append_assoc_cons :: Calculation
append_assoc_cons = prove customLaws "append x (append y (snoc z a)) = append (append x y) (snoc z a)"
  where
    customLaws = [ "++ の最初の定義式: append x nil = x"
                 , "++ の2番目の定義式: append x (snoc y a) = snoc (append x y) a"
                 ]
