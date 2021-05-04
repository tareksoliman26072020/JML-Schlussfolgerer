module JMLTypes where

import Types(Exception,Expression)
 
data JML = JML Syntax1 JML | Done deriving(Show)

data Syntax1 = Normal_Behavior Syntax1
             | Exceptional_Behavior Syntax1
             | Requires JMLExpr
             | Assignable String
             | Ensures JMLExpr
             | Signals_Only Exception
             | Signals String JMLExpr
             | Empty
             deriving(Show)

newtype JMLExpr = JMLExpr Expression deriving(Show,Eq) 
{-
data Syntax2 = Old
             | Result
             | Forall
             | Exists
             | Implication
             | Equivalence
             | Var String
             | IntLiteral Int
             | BoolLiteral Bool
             deriving(Show)-}
