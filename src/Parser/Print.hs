{-# Language LambdaCase, NamedFieldPuns #-}
module Parser.Print where

import Data.Char
import Data.List

import Parser.Types

showType :: Type Types -> String
showType = \case
  BuiltInType t -> map toLower $ show t
  AnyType {typee} -> typee -- ignore generics
  ArrayType {baseType} -> showType baseType ++ "[]"

showExcp :: Exception -> String
showExcp = \case
  UserDefException str -> str
  e -> show e

showExpr :: Expression -> String
showExpr = \case
  VarExpr {varType, varObj, varName} -> maybe "" ((++ " ") . showType) varType
    ++ intercalate "." (varObj
    ++ [if isInfixOf "->" varName then "(" ++ varName ++ ")" else varName])
  BinOpExpr {expr1, binOp, expr2} ->
    showParenExpr expr1 ++ " " ++ show binOp ++ " " ++ showParenExpr expr2
  UnOpExpr {unOp, expr} -> show unOp ++ showParenExpr expr
  IntLiteral num -> show num
  BoolLiteral bool -> map toLower $ show bool
  CharLiteral char -> show char
  StringLiteral str -> show str
  Null -> "null"
  ArrayExpr {arrName, index} -> show arrName
    ++ "[" ++ maybe "" showExpr index ++ "]"
  FunCallExpr {funName, funArgs} -> showExpr funName
    ++ "(" ++ intercalate ", " (map showExpr funArgs) ++ ")"
  CondExpr {eiff, ethenn, eelsee} -> showParenExpr eiff
    ++ "?" ++ showParenExpr ethenn
    ++ ":" ++ showParenExpr eelsee
  AssignExpr {assEleft, assEright} ->
    showExpr assEleft ++ " = " ++ showExpr assEright
  ExcpExpr {excpName, excpmsg} -> "throw new " ++ showExcp excpName ++ "("
    ++ maybe "" show excpmsg ++ ")"
  ReturnExpr {returnE} -> "return" ++ maybe "" ((" " ++) . showExpr) returnE

parenExpr :: Expression -> String
parenExpr = ("(" ++) . (++ ")") . showExpr

showParenExpr :: Expression -> String
showParenExpr e = case e of
  BinOpExpr {} -> parenExpr e
  CondExpr {} -> parenExpr e
  _ -> showExpr e

showModifiers :: [Modifier] -> String
showModifiers = \case
  [] -> ""
  l -> concatMap ((++ " ") . map toLower . show) l

indent :: String -> String
indent = unlines . map ("  " ++) . lines

showExcpType :: Type Exception -> String
showExcpType = \case
  BuiltInType t -> show t
  AnyType {typee, generic} -> typee
    ++ maybe "" ((" " ++) . showExcpType) generic
  ArrayType {baseType} -> showExcpType baseType ++ "[]"

showStmt :: Statement -> String
showStmt = \case
  CompStmt {statements} -> case statements of
    [] -> "{}"
    _ -> "{\n" ++ indent (intercalate ";\n" $ map showStmt statements)
      ++ "}"
  VarStmt {var} -> showExpr var
  AssignStmt {varModifier, assign} ->
    showModifiers varModifier ++ showExpr assign
  CondStmt {condition, siff, selsee} ->
    "if " ++ parenExpr condition ++ " "
    ++ showStmt siff ++ case selsee of
      CompStmt [] -> ""
      _ -> "\nelse " ++ showStmt selsee
  ForStmt {acc, cond, step, forBody} ->
    "for (" ++ showStmt acc ++ "; " ++ showExpr cond ++ "; " ++ showStmt step
    ++ ")\n" ++ showStmt forBody
  WhileStmt {} -> error "showStmt"
  FunCallStmt {funCall} -> showExpr funCall
  TryCatchStmt {tryBody, catchExcp, catchBody, finallyBody} ->
    "try " ++ showStmt tryBody
    ++ "\ncatch (" ++ showExcpType catchExcp ++ ")\n"
    ++ showStmt catchBody ++ case finallyBody of
      CompStmt [] -> ""
      _ -> "\nfinally " ++ showStmt finallyBody
  ReturnStmt {returnS} -> "return" ++ maybe "" ((" " ++) . showExpr) returnS

showDecl :: ExternalDeclaration -> String
showDecl FunDef {funModifier, isPureFlag, funDecl, throws, funBody} =
  showModifiers funModifier
  ++ (if isPureFlag then " /*@ pure @*/ " else "") ++ showStmt funDecl
  ++ maybe "" ((" throws " ++) . showExcp) throws ++ " " ++ showStmt funBody
