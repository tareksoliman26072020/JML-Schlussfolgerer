{-# LANGUAGE NamedFieldPuns #-}
module JML.JMLTypes where

import Parser.Print(showExpr)
import Parser.Types(Exception,Expression(..))
import Prelude hiding(Maybe(..))
import Text.Printf

data JMLSyntax = Normal_Behavior{requires :: JMLExpr,
                                 assignable :: JMLLiterals,
                                 ensures :: JMLExpr} |
                 Exceptional_Behavior{requires :: JMLExpr,
                                      signals :: Exception}
instance Show JMLSyntax where
  show Normal_Behavior{requires, assignable, ensures} =
            "@ normal behavior" ++ "\n" ++
          "  @ requires "   ++ show requires ++ ";\n" ++
          "  @ assignable " ++ show assignable ++ ";\n" ++
          "  @ ensures " ++ "\\result = " ++ show ensures ++ ";\n" ++
          "  @"
  show Exceptional_Behavior{requires, signals} = printf(
            "@ exceptional behavior" ++ "\n" ++
          "  @ requires " ++ show requires ++ ";\n" ++
          "  @ signals "  ++ show signals ++ ";\n" ++
          "  @")

newtype JMLExpr = JMLExpr Expression deriving Eq
instance Show JMLExpr where
  show (JMLExpr expr) = showExpr expr

data JMLLiterals = Old
                 | Result
                 | Forall
                 | Exists
                 | Nothing
                 | Implication
                 | Equivalence

instance Show JMLLiterals where
  show Old = "\\old"
  show Result = "\\result"
  show Forall = "\\forall"
  show Exists = "∃"
  show Nothing = "\\nothing"
  show Implication = "==>"
  show Equivalence = "<==>"
