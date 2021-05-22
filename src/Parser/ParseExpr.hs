module Parser.ParseExpr where

import Data.Char
import Data.List (nub)
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

keywords :: [String]
keywords = words
   $ "abstract   continue   for          new         switch"
  ++ "assert     default    if           package     synchronized"
  ++ "boolean    do         goto         private     this"
  ++ "break      double     implements   protected   throw"
  ++ "byte       else       import       public      throws"
  ++ "case       enum       instanceof   return      transient"
  ++ "catch      extends    int          short       try"
  ++ "char       final      interface    static      void"
  ++ "class      finally    long         strictfp    volatile"
  ++ "const      float      native       super       while"

ident :: Parser String
ident = try $ do
  c <- satisfy (\ c -> isLetter c || c == '_')
  r <- many idChar
  let i = c : r
  if i `elem` keywords then unexpected $ "keyword: " ++ i else skip (return i)

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

parenExpr :: Parser Expression
parenExpr = skipChar '(' *> parseExpr <* skipChar ')'

primExpr :: Parser Expression
primExpr = skip javaLit
  <|> UnOpExpr NotOp <$> (skipChar '!' *> primExpr)
  <|> parenExpr
  <|> Null <$ keyword "null"
  <|> BoolLiteral True <$ keyword "true"
  <|> BoolLiteral False <$ keyword "false"
  <|> parseArray

modifiers :: [Modifier]
modifiers =
  [ Static
  , Public
  , Private
  , Protected
  , Final
  , Abstract ]

parseModifiers :: Parser [Modifier]
parseModifiers = nub <$> many
  (choice $ map (\ m -> m <$ keyword (map toLower $ show m)) modifiers)

parseVar :: Parser Expression
parseVar = do
  t <- parseArrType
  (mt, i) <- case t of
    AnyType i Nothing -> do -- i may be the variable not a type
      m <- optionMaybe ident
      case m of
        Just j -> pure (Just t, j)
        Nothing -> pure (Nothing, i)
    _ -> do
      i <- ident
      pure (Just t, i)
  q <- many $ skipChar '.' *> ident
  let l = i : q
  pure . VarExpr mt (init l) $ last l

parseOptFunCall :: Parser Expression
parseOptFunCall = do
  v <- parseVar
  l <- optionMaybe
    $ skipChar '(' *> sepBy parseExpr (skipChar ',') <* skipChar ')'
  pure $ case l of
    Just args -> FunCallExpr v args
    _ -> v

parseArray :: Parser Expression
parseArray = do
  e <- parseOptFunCall
  l <- optionMaybe
    $ skipChar '[' *> parseExpr <* skipChar ']'
  pure $ case l of
    Nothing -> e
    _ -> ArrayExpr e l

binOps :: [BinOp]
binOps = [ Plus, Mult, Minus, Div, Mod, Less, LessEq, Greater, GreaterEq
  , Eq, Neq, And, Or ]

parseBinOp :: Parser BinOp
parseBinOp =
  choice $ map (\ a -> a <$ keyword (show a)) binOps

parseBinRight :: Parser (BinOp, Expression)
parseBinRight = do
  o <- parseBinOp
  e <- primExpr
  pure (o, e)

parseBinExpr :: Parser Expression
parseBinExpr = do
  e1 <- primExpr
  l <- many parseBinRight
  pure $ foldl (\ a (o, e) -> BinOpExpr a o e) e1 l

parseCondExpr :: Parser Expression
parseCondExpr = do
  e1 <- parseBinExpr
  m <- optionMaybe $ skipChar '?'
  case m of
    Just _ -> do
      e2 <- parseExpr <* skipChar ':'
      CondExpr e1 e2 <$> parseExpr
    _ -> pure e1

parseAssign :: Parser Expression
parseAssign = do
  e1 <- try $ parseArray <* keyword "="
  AssignExpr e1 <$> parseExpr

parseReturn :: Parser Expression
parseReturn = keyword "return" *> (ReturnExpr <$> optionMaybe parseExpr)

parseExcp :: Parser Expression
parseExcp = do
  i <- keyword "throw" *> keyword "new" *> ident
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
  AnyType i <$> optionMaybe (try $ skipChar '<' *> refType <* skipChar '>')

parseType :: Parser (Type Types)
parseType = BuiltInType <$> parseBaseType <|> refType

parseArrType :: Parser (Type Types)
parseArrType = do
  t <- parseType
  l <- many . try $ skipChar '[' *> skipChar ']'
  pure $ foldr (\ _ r -> ArrayType r) t l
