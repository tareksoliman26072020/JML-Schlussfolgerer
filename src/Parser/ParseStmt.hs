module Parser.ParseStmt where

import Data.Maybe

import Parser.ParseExpr
import Parser.Types

import Text.ParserCombinators.Parsec

parseBlock :: Parser Statement
parseBlock = CompStmt <$>
  (skipChar '{' *> many (parseStmt <* many (skipChar ';')) <* skipChar '}')

parseDecl :: Parser Statement
parseDecl = do
  l <- parseModifiers
  e <- parseArray
  m <- optionMaybe $ skipChar '=' *> parseExpr
  pure $ case m of
    Just v -> AssignStmt l (AssignExpr e v)
    _ -> VarStmt e

parseIf :: Parser Statement
parseIf = do
  i <- keyword "if" *> parenExpr
  t <- parseStmt
  m <- optionMaybe $ keyword "else" *> parseStmt
  pure . CondStmt i t $ case m of
    Just e -> e
    _ -> CompStmt []

parseFor :: Parser Statement
parseFor = do
  i <- keyword "for" *> skipChar '(' *> parseStmt <* skipChar ';'
  c <- parseExpr <* skipChar ';'
  s <- parseStmt <* skipChar ')'
  ForStmt i c s <$> parseStmt

parseTry :: Parser Statement
parseTry = do
  t <- keyword "try" *> parseStmt
  e <- keyword "catch" *> skipChar '(' *> ident
  v <- ident
  b <- skipChar ')' *> parseStmt
  m <- optionMaybe $ keyword "finally" *> parseStmt
  pure . TryCatchStmt t (AnyType e . Just $ AnyType v Nothing) b $ case m of
    Just f -> f
    _ -> CompStmt []

exprStmt :: Parser Statement
exprStmt = do
  e <- parseReturn <|> parseExcp <|> parseAssign <|> parseExpr
  case e of
    AssignExpr {} -> pure $ AssignStmt [] e
    VarExpr {} -> pure $ VarStmt e
    FunCallExpr {} -> pure $ FunCallStmt e
    ReturnExpr m -> pure $ ReturnStmt m
    ExcpExpr {} -> pure $ ReturnStmt (Just e)
    _ -> unexpected "expression as statement"

parseStmt :: Parser Statement
parseStmt = parseBlock <|> parseIf <|> parseFor <|> parseTry
  <|> exprStmt <|> parseDecl

parseExtDecl :: Parser ExternalDeclaration
parseExtDecl = do
  l <- parseModifiers
  b <- optionMaybe $ keyword "/*@" *> keyword "pure" <* keyword "@*/"
  t <- parseOptFunCall
  e <- optionMaybe $ keyword "throws" *> (UserDefException <$> ident)
  FunDef l (isJust b) (FunCallStmt t) e <$> parseStmt

parseDeclList :: Parser [ExternalDeclaration]
parseDeclList = spaces *> many parseExtDecl <* eof
