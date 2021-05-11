{-# LANGUAGE NamedFieldPuns #-}
module JML.JMLTypes where

import Parser.Types(Exception,Expression(..),NotatedException(..))
import Prelude hiding(Maybe(..))
import Text.Printf
import Data.List(foldl1',isInfixOf)
import Control.Exception(throw)

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
          "  @ ensures " ++ "\\result=" ++ show ensures ++ ";\n" ++
          "  @"
  show Exceptional_Behavior{requires, signals} = printf(
            "@ exceptional behavior" ++ "\n" ++
          "  @ requires " ++ show requires ++ ";\n" ++
          "  @ signals "  ++ show signals ++ ";\n" ++
          "  @")

newtype JMLExpr = JMLExpr Expression deriving Eq
instance Show JMLExpr where
  show (JMLExpr expr) = showExpr expr where
    showExpr :: Expression -> String
    showExpr VarExpr{varObj=[], varName} = case isInfixOf "->" varName of
      True  -> printf "(%s)" varName
      False -> varName
    showExpr VarExpr{varObj, varName} = foldl1' toString varObj ++ varName where
      toString :: String -> String -> String
      toString "" str = str
      toString acc str = printf "%s.%s" acc str
    showExpr BinOpExpr{expr1, binOp, expr2} = printf "%s%s%s" (showExpr expr1) (show binOp) (showExpr expr2)
    showExpr UnOpExpr{unOp, expr} = printf "%s%s" (show unOp) (show expr)
    showExpr (IntLiteral num) | num>=0 = show num
    showExpr (IntLiteral num) | num<0  = printf "(%d)" num
    showExpr (BoolLiteral bool) = show bool
    showExpr (CharLiteral char) = show char
    showExpr (StringLiteral str) = str
    showExpr Null = "null"
    showExpr a = throw $ NoteExcp $ printf "\ninstance Show JMLExpr:\n%s" (show a)

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
