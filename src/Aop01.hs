module Aop01 where

import Calculations (Calculation)
import Simplify (prove, simplify)

append_assoc_nil :: Calculation
append_assoc_nil = prove customLaws "append x (append y nil) = append (append x y) nil"
  where
    customLaws = [ "++ の定義(基底): append x nil = x"
                 , "++ の定義(再帰): append x (snoc y a) = snoc (append x y) a"
                 ]

append_assoc_snoc :: Calculation
append_assoc_snoc = prove customLaws "append x (append y (snoc z a)) = append (append x y) (snoc z a)"
  where
    customLaws = [ "++ の定義(基底): append x nil = x"
                 , "++ の定義(再帰): append x (snoc y a) = snoc (append x y) a"
                 , "帰納法の仮定: append x (append y z) = append (append x y) z"
                 ]
