module Parser.ParseExpr where

import Data.Char
import Parser.Types

import Text.ParserCombinators.Parsec

skip :: Parser a -> Parser a
skip = (<* spaces)

signs :: String
signs = ".+-/*<>=!%|&?:"

keyword :: String -> Parser String
keyword s = skip . try $ string s <* notFollowedBy
  (if all idCh s then idChar else oneOf signs)

idCh :: Char -> Bool
idCh c = isAlphaNum c || c `elem` "$_"

idChar :: Parser Char
idChar = satisfy idCh

ident :: Parser String
ident = skip $ many1 idChar

intLit :: Parser Int
intLit = do
  m <- optionMaybe $ char '-'
  i <- many1 $ satisfy isDigit
  pure . maybe id (const negate) m $ read i

charLit :: Parser Char
charLit = char '\'' *> ((char '\\' *> oneOf "\\'") <|> noneOf "'") <* char '\''

stringLit :: Parser String
stringLit =
  char '"' *> many ((char '\\' *> oneOf "\\\"") <|> noneOf "\"") <* char '"'

javaLit :: Parser Expression
javaLit = IntLiteral <$> intLit
  <|> CharLiteral <$> charLit
  <|> StringLiteral <$> stringLit

skipChar :: Char -> Parser Char
skipChar = skip . char

primExpr :: Parser Expression
primExpr = skip javaLit
  <|> UnOpExpr NotOp <$> (skipChar '!' *> primExpr)
  <|> skipChar '(' *> parseExpr <* skipChar ')'
  <|> Null <$ keyword "null"
  <|> BoolLiteral True <$ keyword "true"
  <|> BoolLiteral False <$ keyword "false"
  <|> parseArray

-- do not parse leading Type
parseOptFunCall :: Parser Expression
parseOptFunCall = do
  s <- ident
  l <- optionMaybe
    $ skipChar '(' *> sepBy parseExpr (skipChar ',') <* skipChar ')'
  case l of
    Just args -> pure $ FunCallExpr (VarExpr Nothing [] s) args
    _ -> do
      l <- many $ skipChar '.' *> ident
      pure $ VarExpr Nothing l s

parseArray :: Parser Expression
parseArray = do
  e <- parseOptFunCall
  l <- optionMaybe
    $ skipChar '[' *> parseExpr <* skipChar ']'
  pure $ case l of
    Just arg -> ArrayExpr e $ Just arg
    _ -> e

binOps :: [BinOp]
binOps = [ Plus, Mult, Minus, Div, Mod, Less, LessEq, Greater, GreaterEq
  , Eq, Neq, And, Or ]

parseBinOp :: Parser BinOp
parseBinOp =
  choice $ map (\ a -> a <$ keyword (filter (/= ' ') $ show a)) binOps

parseBinExpr :: Parser Expression
parseBinExpr = do
  e1 <- primExpr
  m <- optionMaybe parseBinOp
  case m of
    Just o -> do
      e2 <- parseBinExpr
      pure $ BinOpExpr e1 o e2
    _ -> pure e1

parseCondExpr :: Parser Expression
parseCondExpr = do
  e1 <- parseBinExpr
  m <- optionMaybe $ skipChar '?'
  case m of
    Just _ -> do
      e2 <- parseExpr <* skipChar ':'
      e3 <- parseExpr
      pure $ CondExpr e1 e2 e3
    _ -> pure e1

parseAssign :: Parser Expression
parseAssign = do
  e1 <- try $ parseArray <* skipChar '='
  AssignExpr e1 <$> parseExpr

parseReturn :: Parser Expression
parseReturn = keyword "return" *> (ReturnExpr <$> optionMaybe parseExpr)

parseExcp :: Parser Expression
parseExcp = do
  keyword "throw"
  keyword "new"
  i <- ident
  ExcpExpr (UserDefException i) <$> optionMaybe
    (skipChar '(' *> skip stringLit <* skipChar ')')

parseExpr :: Parser Expression
parseExpr = parseAssign <|> parseCondExpr

baseTypes :: [Types]
baseTypes = [Int, Void, Char, Double, Short, Float, Long, Boolean, Byte]

parseBaseType :: Parser Types
parseBaseType =
  choice $ map (\ a -> a <$ keyword (map toLower $ show a)) baseTypes

refType :: Parser (Type Types)
refType = do
  i <- ident
  AnyType i <$> optionMaybe (skipChar '<' *> refType <* skipChar '>')

parseType :: Parser (Type Types)
parseType =
  BuiltInType <$> parseBaseType
  <|> refType

parseArrType :: Parser (Type Types)
parseArrType = do
  t <- parseType
  l <- many (skipChar '[' *> skipChar ']')
  pure $ foldr (\ _ r -> ArrayType r) t l
