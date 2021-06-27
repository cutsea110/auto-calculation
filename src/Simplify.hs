module Simplify
  ( simplify
  , prove
  ) where

import Calculations (Calculation, calculate, paste)
import Parsing
import Laws
import Expressions

-- TODO: ここで Laws の変数をチェック(練習問題 D)
simplify :: [String] -> String -> Calculation
simplify strings string = let laws = map (parse law) strings

                              e = parse expr string
                          in calculate laws e

-- TODO: ここで Laws の変数をチェック(練習問題 D)
prove :: [String] -> String -> Calculation
prove strings string = let laws = map (parse law) strings
                           (e1, e2) = parse equation string
                       in paste (calculate laws e1) (calculate laws e2)
