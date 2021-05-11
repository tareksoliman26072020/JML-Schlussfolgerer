module Parser.ParseExpressions where

import Parser.Types
import Parser.Parser
import Parser.PrimitiveFunctionality

import Control.Applicative
import Control.Monad
import Data.Char(isAlphaNum,isSpace,isDigit)
import Data.List.Split(splitOn)
import Data.List(isPrefixOf,(\\),elemIndices)
import Data.Maybe(fromJust,fromMaybe,isJust,isNothing)
import Data.Foldable(asum)
import Control.Monad.State (runStateT)

--parseLiteral :: Parser Expression
--parseLiteral = whitespace_linebreak *> many (satisfy (not.isSpace)) >>= return . Literal

parseModifier :: Parser [Modifier]
parseModifier =
  whitespace_linebreak *> liftA (map toModifier)
    (some (token "public"    <|>
           token "private"   <|>
           token "protected" <|>
           token "final"     <|>
           token "abstract"  <|>
           token "static"))

parseType :: Parser (Type Types)
parseType =
  whitespace_linebreak *>
    liftA toType (many (satisfy (/= ' ')))
      <* whitespace_linebreak

parseVariableExpr :: String -> Parser Expression
parseVariableExpr strr = do
  maybeModifiers <- optional parseModifier
  res <- some(takeUntilFirstOccurrence strr)
  let strToParse = callFunUntil ((/=' ') . last) init $
                     f1 $ removeInfixDublicates ' ' (dropWhile isSpace res)
      Just (x,_) = if length (elemIndices ' ' strToParse) == 1 then runStateT f2 strToParse
                   else runStateT f3 strToParse
  return x
  where
    -- adjust string so that unexcepted whitespaces are eliminated
    f1 :: String -> String
    f1 str =
      let foo "" res = res
          foo (x:rest) res
            | not (null rest) && isSpace x && (head rest == '.' || head rest == '<' || head rest == '>') = foo rest res
            | not (null rest) && (x == '.' || x == '<') && isSpace (head rest) = foo (tail rest) (res ++ [x])
            | otherwise = foo rest (res ++ [x])
      in foo str ""

    -- parse to expression for VarExpr
    f2 :: Parser Expression
    f2 = do
      typ <- whitespace_linebreak *> parseType
      maybeObjs <- optional(some $ takeUntilLastOccurrence ".")
      optional $ char '.'
      name <- many $ satisfy (const True)
      return $ VarExpr {varType = Just typ,
                        varObj = if isNothing maybeObjs then []
                                 else splitOn "." (fromJust maybeObjs),
                        varName = name}

    f3 :: Parser Expression
    f3 = do
      maybeObjs <- optional(some $ takeUntilLastOccurrence ".")
      optional $ char '.'
      name <- many $ satisfy (const True)
      return $ VarExpr {varType = Nothing,
                        varObj = if isNothing maybeObjs then []
                                 else splitOn "." (fromJust maybeObjs),
                        varName = name}
      --return $ StringLiteral "meow"


-- BinOpExpr {expr1 :: Expression, binOp :: BinOp, expr2 :: Expression}
-- data BinOp = Plus | Mult | Minus | Div | LessEq | Eq | And | Or deriving (Eq, Show)
parseBinOp :: String -> Parser Expression
parseBinOp strr = do
  whitespace_linebreak
  expr <- (many $ takeUntilFirstOccurrence strr)
  let x = runStateT res (expr++";")
  if isNothing x then failure
  else return $ fst $ fromJust x
  where
    res = do
        exp1 <- asum (parseInt : parseBool : parseChar : parseString : parseArray :
                foldl(\l r -> l <|> parseFunCallExpr r) failure binOps :
                foldl(\l r -> l <|> parseUnOp r) failure binOps :
                foldl(\l r -> l <|> parseVariableExpr r) failure binOps : [])
        whitespace_linebreak
        binop <- foldl (\l str -> l <|> token str) failure binOps
        exp2  <- parseBinOp ";" <|> parseInt <|> parseBool <|> parseChar <|> parseString <|> parseArray <|>
                 parseFunCallExpr ";" <|> parseUnOp ";" <|> parseVariableExpr ";"
        --optional(token strr)
        return $ BinOpExpr {expr1 = exp1, binOp = toBin binop, expr2 = exp2}

parseUnOp :: String -> Parser Expression
parseUnOp strr = do
  whitespace_linebreak
  unop <- char '!'
  exp  <- parseBool <|> parseBinOp strr <|> parseVariableExpr strr
  --token strr
  return $ UnOpExpr {unOp = NotOp, expr = exp}

parseInt :: Parser Expression
parseInt = (do
  whitespace_linebreak
  n <- some $ satisfy (isDigit)
  return $ IntLiteral (read n :: Int)) <|> do
    token "-"
    n <- some $ satisfy (isDigit)
    return $ IntLiteral $ negate (read n :: Int)

parseBool :: Parser Expression
parseBool = do
  whitespace_linebreak
  n <- optional $ token "true" <|> token "false"
  if isNothing n then failure
  else return $ BoolLiteral $ if n == Just "true" then True else False

parseChar :: Parser Expression
parseChar = do
  whitespace_linebreak
  char '\''
  ch <- satisfy (const True)
  char '\''
  return $ CharLiteral ch

parseString :: Parser Expression
parseString = do
  whitespace_linebreak
  char '\"'
  str <- many (satisfy (/='\"'))
  char '\"'
  return $ StringLiteral str

parseNull :: Parser Expression
parseNull = fmap (const Null) (token "null")

-- ArrayExpr {arrName :: Expression, index :: Expression}
-- TODO: return to change idx
parseArray :: Parser Expression
parseArray = do
  whitespace_linebreak
  name <- parseFunCallExpr "[" <|> parseVariableExpr "["
  token "["
  temp <- item'
  if(temp == ']') then (token "]" *> (return $ ArrayExpr {arrName = name, index = Nothing}))
  else do
    idx1 <- optional (parseBinOp "]" <|> parseInt <|> parseFunCallExpr "]")
    idx <- if isJust idx1 then return (fromJust idx1) else parseVariableExpr "]"
    token "]"
    return $ ArrayExpr {arrName = name, index = Just idx}

-- FunCallExpr {funName :: Expression, funargs :: [Expression], throws :: Maybe Exception}
parseFunCallExpr :: String -> Parser Expression
parseFunCallExpr strr = do
  whitespace_linebreak
  namee <- many $ satisfy(\ch-> ch /= ' ' && ch /= '(')--parseVariableExpr "("
  let name = runStateT (parseVariableExpr "(") (namee++"(")
  if isNothing name then failure
  else do
    char '('
    unrefined <- many $ takeUntilFirstOccurrence strr
    guard(f2 unrefined)
    let splitted = splitOn "," (f3 unrefined)
        listArgs = if length splitted == 1 && length (head splitted) == 0 then []
                   else map f splitted
    return $ FunCallExpr {funName = fst $ fromJust name, funArgs = listArgs}
  where
    f :: String -> Expression
    f str =
      let parsing1 = parseBinOp "" <|> parseUnOp "" <|> parseInt <|> parseString <|> parseBool <|> parseChar <|> parseArray
          parsed   = runStateT parsing1 str <|>
                     runStateT (parseFunCallExpr ";") (str++";") <|>
                     runStateT (parseVariableExpr ";") (str++";")
      in fst $ fromJust parsed

    -- if False, then this is not a function
    -- this puts into consideration whether there are functions passed as arguments
    f2 :: String -> Bool
    f2 str = foldl (\l r->
      if r=='(' then l+1 else
      if r==')' then l-1
      else l) 1 str == 0

    -- remove the outer last bracet of the function
    f3 :: String -> String
    f3 str = f3' str "" where
      f3' "" res = res
      f3' (x:rest) res
        | x == ')' && notElem ')' rest = res
        | otherwise = f3' rest (res++[x])

-- CondExpr {eiff :: Expression, ethenn :: Expression, eelsee :: Expression}
parseCondExpr :: Parser Expression
parseCondExpr = do
  to_1 <- takeUntilChar '?'
  let _1 = fst $ fromJust $ runStateT (parseBinOp "") to_1
  whitespace_linebreak *> char '?' <* whitespace_linebreak
  _2 <- parseCondExpr <|>
        parseInt <|> parseBool <|> parseChar <|> parseString <|>
        parseFunCallExpr ":" <|>
        parseArray <|>
        parseUnOp ":" <|>
        parseReturnExpr <|>
        parseExcp <|>
        parseVariableExpr ":"
        <* whitespace_linebreak
  optional(token ":")
  _3 <- parseCondExpr <|>
        parseInt <|> parseBool <|> parseChar <|> parseString <|>
        parseFunCallExpr ";" <|>
        parseArray <|>
        parseUnOp ";" <|>
        parseReturnExpr <|>
        parseExcp <|>
        parseVariableExpr ";"
        <* whitespace_linebreak
  return $ CondExpr {eiff = _1, ethenn = _2, eelsee = _3}

parseReturnExpr :: Parser Expression
parseReturnExpr = (do
  token "return"
  maybeDone <- optional $ token ";"
  if isJust maybeDone then return $ ReturnExpr Nothing
  else do
    res <- parseBinOp ";" <|> parseUnOp ";" <|> parseArray <|> parseFunCallExpr ";" <|>
           parseInt <|> parseChar <|> parseString <|> parseBool <|> parseNull <|> parseVariableExpr ";"
    optional $ token ";"
    return $ ReturnExpr (Just res)) <|> fmap (ReturnExpr . Just) parseExcp

parseAssignExpr :: Parser Expression
parseAssignExpr = do
  whitespace_linebreak
  state <- getState'
  expr <- takeUntilChar ';'
  let parsed = runStateT parseAssignExpr' (expr ++ ";")
  if isNothing parsed then failure
  else newState (state \\ expr) *> return (fst $ fromJust parsed)
  where
    parseAssignExpr' :: Parser Expression
    parseAssignExpr' = do
      left <- parseVariableExpr "="
      token "="
      preAfter <- getStatePrefix
      guard(preAfter /= '=')
      right <- parseBinOp ";" <|> parseUnOp ";" <|>
               parseInt <|> parseBool <|> parseString <|> parseArray <|>
               parseFunCallExpr ";" <|> parseCondExpr <|> parseVariableExpr ";"
      return $ AssignExpr {assEleft = left, assEright = right}

-- ExcpExpr {excpName :: Exception, excpmsg :: Maybe String}
parseExcp :: Parser Expression
parseExcp = do
  token "throw"
  token "new"
  excp <- token "Exception"                       <|>
          token "ArithmeticException"             <|>
          token "ArrayIndexOutOfBoundsException"  <|>
          token "ClassNotFoundException"          <|>
          token "FileNotFoundException"           <|>
          token "IOException"                     <|>
          token "InterruptedException"            <|>
          token "NoSuchFieldException"            <|>
          token "NoSuchMethodException"           <|>
          token "NullPointerException"            <|>
          token "NumberFormatException"           <|>
          token "RuntimeExceptionException"       <|>
          token "StringIndexOutOfBoundsException" <|>
          token "IllegalArgumentException"        <|>
          many (satisfy (\ch -> ch /= ' ' && ch /= '(' && ch /= ';'))
  token "("
  maybeBrace2 <- optional(char ')')
  if isJust maybeBrace2 then return $ ExcpExpr {excpName = toException excp, excpmsg = Nothing}
  else (many(satisfy (/= ')')) >>= \msg -> return $ ExcpExpr {excpName = toException excp, excpmsg = Just (msg \\ replicate (length msg) '\"')}) <* char ')'
