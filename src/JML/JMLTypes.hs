{-# LANGUAGE NamedFieldPuns #-}
module JML.JMLTypes where

import Parser.Print(showType)
import Parser.Types(BinOp(..),Exception,Expression(..), NotatedException(..))
import qualified Control.Exception
import Prelude hiding(Nothing)
import Text.Printf
import Data.List
import Data.Char
import Data.Maybe(fromJust)

data JMLSyntax = Normal_Behavior{requires :: JMLExpr,
                                 assignable :: JMLLiterals,
                                 ensures :: JMLExpr} |
                 Exceptional_Behavior{requires :: JMLExpr,
                                      signals :: Exception}
instance Show JMLSyntax where
  show Normal_Behavior{requires, assignable, ensures} =
            "@ normal behavior" ++ "\n" ++
          "  @ requires "   ++  ppRequires (show requires) ++ ";\n" ++
          "  @ assignable " ++ show assignable ++ ";\n" ++
          "  @ ensures " ++ "\\result = " ++ ppEnsures (show ensures) ++ ";\n" ++ "  @"
  show Exceptional_Behavior{requires, signals} = printf(
            "@ exceptional behavior" ++ "\n" ++
          "  @ requires " ++ ppRequires (show requires) ++ ";\n" ++
          "  @ signals "  ++ show signals ++ ";\n" ++
          "  @")

ppJML :: Int -> String -> String -> String
ppJML indent input output =
  let tuple = span (/= '&') input
  in case tuple of
       (first,second) | "&&" `isInfixOf` second -> ppJML indent (tail $ tail second) (output ++ first ++ "&&" ++ "\n" ++ "  @" ++ replicate indent ' ')
       (first,second)                           -> output ++ first

ppEnsures :: String -> String
ppEnsures input = ppEnsures' input ""
  where ppEnsures' inp outp = ppJML 8 inp outp

ppRequires :: String -> String
ppRequires input = ppRequires' input ""
  where ppRequires' inp outp = ppJML 9 inp outp



newtype JMLExpr = JMLExpr Expression deriving Eq
instance Show JMLExpr where
  show (JMLExpr expr') = case expr' of
    VarExpr {varObj, varName} -> intercalate "." (varObj ++ [varName])
    a@BinOpExpr{}
      | binOp a `elem` [Eq,Neq,Less,LessEq,Greater,GreaterEq,Plus,Minus,Mult,Div] ->
          show (JMLExpr $ expr1 a) ++ show (binOp a) ++ show (JMLExpr $ expr2 a)
      | binOp a `elem` [And,Or] ->
          show (JMLExpr $ expr1 a) ++ " " ++ show (binOp a) ++ " " ++ show (JMLExpr $ expr2 a)
    UnOpExpr {unOp, expr} -> (show unOp) ++ (show $ JMLExpr expr)
    IntLiteral num | num >= 0 -> show num
    IntLiteral num -> "(" ++ show num ++ ")"
    BoolLiteral bool -> map toLower $ show bool
    CharLiteral char -> show char
    StringLiteral str | "this" `isInfixOf` str -> "(" ++ str ++ ")"
    StringLiteral str -> show str
    AssignExpr {assEleft, assEright} ->
        (show $ JMLExpr assEleft) ++ "=" ++ (show $ JMLExpr assEright)

data JMLLiterals = Old
                 | Result
                 | Forall
                 | Exists
                 | Nothing
                 | Implication
                 | Equivalence
                 | Assigned [String]

instance Show JMLLiterals where
  show Old = "\\old"
  show Result = "\\result"
  show Forall = "\\forall"
  show Exists = "∃"
  show Nothing = "\\nothing"
  show Implication = "==>"
  show Equivalence = "<==>"
  show (Assigned []) = show Nothing
  show (Assigned list) = intercalate ", " list