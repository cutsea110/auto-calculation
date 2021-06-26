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

test :: Calculation
test = prove customLaws "listr outl . filter outr . zip . pair (id, listr p) = filter p"

-- ^ bool(McCarthy条件式)の定義: bool p f g a = if p a then f a else g a
customLaws = [ "filterの定義: filter p = concat . listr (p -> wrap , nilp)"
             , "等式(1.1): outl . pair (f , g) = f"
             , "等式(1.2): outr . pair (f , g) = g"
             , "等式(1.3): nilp . f = nilp"
             , "等式(1.4): listr f . nilp = nilp"
             , "等式(1.5): listr f . wrap = wrap . f"
             , "等式(1.6): listr f . concat = concat . listr (listr f)"
             , "等式(1.7): zip . pair (listr f , listr g) = listr (pair (f , g))"
             , "等式(1.8): listr (f . g) = listr f . listr g"
             , "等式(1.9): listr id = id"
             , "等式(1.10): (p -> f , g) . h = (p . h -> f . h , g . h)"
             , "等式(1.11): h . (p -> f , g) = (p -> h . f , h . g)"
             , "等式(1.12): f . id = f"
             , "等式(1.13): id . f = id"
             ]
