module Calculations
  ( Calculation (Calc), Step, calculate, paste ) where

import Expressions
import Laws
import Rewrites
import Utilities (compose)


data Calculation = Calc Expr [Step]
type Step = (LawName, Expr, Direction)
data Direction = Forward | Backward deriving Show

opposite :: Direction -> Direction
opposite Forward  = Backward
opposite Backward = Forward

calculate :: [Law] -> Expr -> Calculation
calculate laws e = Calc e (manyStep rws (e, Forward))
  where
    rws (e, d) = [ (name, e', d)
                 | Law name eqn <- sortedlaws
                 , e' <- rewrites eqn e
                 , e' /= e
                 ]
    sortedlaws = sortLaws laws

manyStep :: ((Expr, Direction) -> [Step]) -> (Expr, Direction) -> [Step]
manyStep rws e = if null steps then []
                 else step : manyStep rws (tl step)
  where
    steps = rws e
    step = head steps
    tl (_, y, z) = (y, z)


instance Show Calculation where
  showsPrec _ (Calc e steps) = showString "\n " . shows e . showChar '\n' . compose (map showStep steps)

showStep :: Step -> ShowS
showStep (why, e, d) = showString "= {" . showString why . showDirection d . showString "}\n " . shows e . showChar '\n'
  where
    showDirection Forward = (""++)
    showDirection Backward = (" [Backward]"++)

reverseCalc :: Calculation -> Calculation
reverseCalc (Calc e steps) = foldl shunt (Calc e []) steps
  where shunt (Calc e1 steps') (why, e2, dir)
          = Calc e2 ((why, e1, opposite dir) : steps')

paste :: Calculation -> Calculation -> Calculation
paste calc1@(Calc e1 steps1) calc2 = if conc1 == conc2
                                     then Calc e1 (prune conc1 rsteps1 rsteps2)
                                     else Calc e1 (steps1 ++ (gap, conc2, Forward) : rsteps2) -- 方向は Don't care
  where
    Calc conc1 rsteps1 = reverseCalc calc1
    Calc conc2 rsteps2 = reverseCalc calc2
    gap = "... ??? ..."

prune :: Expr -> [Step] -> [Step] -> [Step]
prune e ((_, e1, d1) : steps1) ((_, e2, _) : steps2)
  | e1 == e2 = prune e1 steps1 steps2
prune e steps1 steps2 = rsteps ++ steps2
  where
    Calc _ rsteps = reverseCalc (Calc e steps1)
