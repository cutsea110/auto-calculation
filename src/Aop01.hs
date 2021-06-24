module Aop01 where

import Calculations (Calculation)
import Simplify (prove, simplify)

append_assoc_nil :: Calculation
append_assoc_nil = prove customLaws "x ++ (y ++ nil) = (x ++ y) ++ nil"
  where
    customLaws = [ "++ の定義(基底): x ++ nil = x"
                 , "++ の定義(再帰): x ++ (snoc y a) = snoc (x ++ y) a"
                 ]

append_assoc_snoc :: Calculation
append_assoc_snoc = prove customLaws "x ++ (y ++ (snoc z a)) = (x ++ y) ++ (snoc z a)"
  where
    customLaws = [ "++ の定義(基底): x ++ nil = x"
                 , "++ の定義(再帰): x ++ (snoc y a) = snoc (x ++ y) a"
                 , "帰納法の仮定: x ++ (y ++ z) = (x ++ y) ++ z"
                 ]
