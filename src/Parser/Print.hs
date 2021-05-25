{-# Language LambdaCase, NamedFieldPuns #-}
module Parser.Print where

import Data.Char
import Data.List

import Parser.Types

-- if xss contains a string whose infix starts with any mentioned string in except, then this element in xss will be handled with sep2
-- otherwise it will be handled with sep1
intercalate' :: String -> String -> [String] -> [String] -> String
intercalate' sep1 sep2 except xss = concat (intersperse' sep1 sep2 except xss)
  where
    intersperse' _    _    except []        = []
    intersperse' sep1 sep2 except a  = prependToAll' sep1 sep2 except a

    prependToAll' _    _    _      [] = []
    prependToAll' sep1 sep2 except (x:xs)
      | any (`isInfixOf` x) except = x : sep2 : prependToAll' sep1 sep2 except xs
      | otherwise                     =  x : sep1 : prependToAll' sep1 sep2 except xs

showType :: Type Types -> String
showType = \case
  BuiltInType t -> map toLower $ show t
  AnyType {typee} -> typee -- ignore generics
  ArrayType {baseType} -> showType baseType ++ "[]"

showExcp :: Exception -> String
showExcp (Exception str) = str

showExpr :: Expression -> String
showExpr = \case
  VarExpr {varType, varObj, varName} -> maybe "" ((++ " ") . showType) varType
    ++ intercalate "." (varObj ++ [varName])
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
    _ -> "{\n" ++ indent (intercalate' ";\n" "\n" except (map showStmt statements))
      ++ "}"
    where
      except = words "for if else while try catch finally"
  VarStmt {var} -> showExpr var
  AssignStmt {varModifier, assign} ->
    showModifiers varModifier ++ showExpr assign
  CondStmt {condition, siff, selsee} ->
    "if" ++ parenExpr condition
    ++ showStmt siff ++ case selsee of
      CompStmt [] -> ""
      _ -> "\nelse" ++ showStmt selsee
  ForStmt {acc, cond, step, forBody} ->
    "for(" ++ showStmt acc ++ "; " ++ showExpr cond ++ "; " ++ showStmt step
    ++ ")" ++ showStmt forBody
  WhileStmt {} -> error "showStmt"
  FunCallStmt {funCall} -> showExpr funCall
  TryCatchStmt {tryBody, catchExcp, catchBody, finallyBody} ->
    "try" ++ showStmt tryBody
    ++ "\ncatch(" ++ showExcpType catchExcp ++ ")"
    ++ showStmt catchBody ++ case finallyBody of
      CompStmt [] -> ""
      _ -> "\nfinally " ++ showStmt finallyBody
  ReturnStmt {returnS} -> case returnS of
    Just e@ExcpExpr {} -> showExpr e
    _ -> showExpr (ReturnExpr returnS)

showDecl :: ExternalDeclaration -> String
showDecl FunDef {funModifier, isPureFlag, funDecl, throws, funBody} =
  showModifiers funModifier
  ++ (if isPureFlag then "/*@ pure @*/ " else "") ++ showStmt funDecl
  ++ maybe "" ((" throws " ++) . showExcp) throws ++ showStmt funBody