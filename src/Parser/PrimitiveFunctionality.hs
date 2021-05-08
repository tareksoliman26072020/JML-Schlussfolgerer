{-# Language LambdaCase #-}
module PrimitiveFunctionality where

import Parser
import Types

import Control.Applicative
import Control.Monad
import Data.List (uncons, isPrefixOf, (\\), isInfixOf, elemIndices)
import Data.Char (isSpace,toLower)
import Data.List.Split(splitOn)

-- | Parser, der ein Zeichen der Eingabe konsumiert
item :: Parser Char
item = Parser uncons

item' :: Parser Char
item' = Parser $ \case str | null str -> Nothing; str@(x:_) -> Just(x,str)

-- | Parser, der ein Zeichen der Eingabe konsumiert,
-- sollte p für dieses Zeichen halten. Ansonsten schlägt das Parsieren fehl.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then pure c else failure

-- parse (many $ takeUntilFirstOccurrence "==") "xxxx ==" ~~> 
--   Just ("xxxx"," ==")
takeUntilFirstOccurrence :: String -> Parser Char
takeUntilFirstOccurrence strr = {-item >>= \c -> -}Parser $ \str ->
  if not (isInfixOf strr str) then Nothing else
  if isPrefixOf strr str then Nothing
  else Just(head str,tail str)

takeUntilLastOccurrence :: String -> Parser Char
takeUntilLastOccurrence strr = Parser $ \str ->
  if (not $ isInfixOf strr str) ||
     ((not $ null str) && (not $ isInfixOf strr (tail str))) then Nothing
  else Just(head str,tail str)

isSubString :: String -> Parser Bool
isSubString strr = Parser $ \str ->
  Just(isInfixOf strr str,"")
  
getState :: Parser a -> Parser (a,String)
getState parser = Parser $ \str -> case parse parser str of
  Nothing      -> Nothing
  Just(a,rest) -> Just((a,rest),rest)

getState' :: Parser String
getState' = Parser (Just . join (,))
--getState' = Parser $ \str -> (str,str)

getStatePrefix :: Parser Char
getStatePrefix = Parser $ \str -> case null str of True -> Nothing; _ -> Just(head str,str)

newState :: String -> Parser String
newState str = Parser $ \_ -> Just(str,str)

-- | Parser, der nur c parsiert und ansonsten fehlschlägt
char :: Char -> Parser Char
char c = satisfy (==c)

-- | Parsiert die übergebene Zeichenkette
keyword :: String -> Parser String
keyword = mapM char

whitespace :: Parser ()
whitespace = void . many $ satisfy isSpace

linebreak :: Parser ()
linebreak = void . many $ satisfy (=='\n')

whitespace_linebreak :: Parser()
whitespace_linebreak = void . many $ satisfy (\ch -> isSpace ch || ch == '\n')

whitespace_linebreak_semiColon :: Parser()
whitespace_linebreak_semiColon = void . many $ satisfy (\ch -> isSpace ch || ch == '\n' || ch == ';')

token :: String -> Parser String
token s = whitespace_linebreak *> keyword s <* whitespace_linebreak

takeUntilChar :: Char -> Parser String
takeUntilChar ch = Parser $ \str ->
  if notElem ch str then Nothing
  else let splitted = splitOn [ch] str
       in Just(head splitted,str \\ head splitted)

isPrefix :: String -> Parser Bool
isPrefix strr = Parser $ \str ->
  Just(isPrefixOf strr str,"")

isPrefix' :: String -> Parser Bool
isPrefix' strr = whitespace_linebreak *> Parser (\str ->
  Just(isPrefixOf strr str,str))

isInfix' :: String -> Parser Bool
isInfix' strr = Parser $ \str -> Just(isInfixOf strr str,str)
  
transform :: (a -> b) -> Parser a -> Parser b
transform f (Parser p) = Parser $ \str -> case p str of
  Nothing        -> Nothing
  Just (x, rest) -> Just (f x,rest)

-- take ch1 from str before reaching ch2 in str
takeOnlyIfBefore :: Char -> Char -> Parser String
takeOnlyIfBefore ch1 ch2 = Parser $ \str -> case elem ch2 str of
  True  ->
    let first1 = head $ splitOn [ch2] str
        rest2  = last $ splitOn [ch2] str
    in case elem ch1 first1 of
      True  ->
        let first2 = head $ splitOn [ch1] first1
            rest1  = last $ splitOn [ch1] first1
        in Just(first2,rest1 ++ [ch2] ++ rest2)
      False -> Just("",str)
  False -> Just("",str)

empty :: Parser String
empty = return ""

elemInState :: Char -> Parser a -> Parser Bool
elemInState ch p = Parser $ \str -> case parse p str of
  Nothing -> Just(False,"")
  Just(_,str2) -> Just(elem ch str2,"")


removeInfixDublicates :: Eq a => a -> [a] -> [a]
removeInfixDublicates elm list = foo elm list 0 [] where
  foo _   []       _ res            = res
  foo elm (x:rest) 0 res | elm == x = foo elm rest 1 (res ++ [x])
  foo elm (x:rest) i res | elm /= x = foo elm rest 0 (res ++ [x])
  foo elm (x:rest) i res | elm == x = foo elm rest i res

callFunUntil :: (a -> Bool) -> (a -> a) -> a -> a
callFunUntil p f x
  | (not $ p x) = callFunUntil p f (f x)
  | otherwise = x

deleteAllOccurrences :: Eq a => a -> [a] -> [a]
deleteAllOccurrences elm list
  | elem elm list = list \\ replicate (length list) elm
  | otherwise     = list

toModifier :: String -> Modifier
toModifier "public"    = Public
toModifier "private"   = Private
toModifier "protected" = Protected
toModifier "final"     = Final
toModifier "abstract"  = Abstract
toModifier "static"    = Static

fromModifier :: Modifier -> String
fromModifier modifier = (toLower $ head $ show modifier):(tail $ show modifier)

fromModifiers :: [Modifier] -> String
fromModifiers modifiers = unwords $ map fromModifier modifiers

toType :: String -> Type Types
toType "byte" = BuiltInType Byte
toType "char" = BuiltInType Char
toType "short" = BuiltInType Short
toType "int" = BuiltInType Int
toType "long" = BuiltInType Long
toType "float" = BuiltInType Float
toType "double" = BuiltInType Double
toType "boolean" = BuiltInType Boolean
toType "void" = BuiltInType Void
toType str | elem '[' str    = ArrayType {baseType = toType $ head $ splitOn "[" str}
toType str | notElem '<' str = AnyType {typee = str, generic = Nothing}
toType str | elem '<' str    =
  let splittedBefore = splitOn "<" str
      splittedAfter  = splitOn ">" (last splittedBefore)
  in AnyType {typee = head splittedBefore \\ replicate (length $ elemIndices ' ' $ head splittedBefore) ' ',
              generic = Just $ toType $ head splittedAfter}
--toType str | isPrefixOf "Exception" str = BuiltInType (Exception $ toException str)

toException :: String -> Exception
toException "Exception"                       = Exception
toException "ArithmeticException"             = ArithmeticException
toException "ArrayIndexOutOfBoundsException"  = ArrayIndexOutOfBoundsException
toException "ClassNotFoundException"          = ClassNotFoundException
toException "FileNotFoundException"           = FileNotFoundException
toException "IOException"                     = IOException
toException "InterruptedException"            = InterruptedException
toException "NoSuchFieldException"            = NoSuchFieldException
toException "NoSuchMethodException"           = NoSuchMethodException
toException "NullPointerException"            = NullPointerException
toException "NumberFormatException"           = NumberFormatException
toException "RuntimeExceptionException"       = RuntimeExceptionException
toException "StringIndexOutOfBoundsException" = StringIndexOutOfBoundsException
toException "IllegalArgumentException"        = IllegalArgumentException
toException str                               = UserDefException str

-- data BinOp = Plus | Mult | Minus | Div | LessEq | Eq | Neq | And | Or deriving (Eq, Show)
toBin :: String -> BinOp
toBin "+"  = Plus
toBin "*"  = Mult
toBin "-"  = Minus
toBin "/"  = Div
toBin "%"  = Mod
toBin "<"  = Less
toBin "<=" = LessEq
toBin ">"  = Greater
toBin ">=" = GreaterEq
toBin "==" = Eq
toBin "!=" = Neq
toBin "&&" = And
toBin "||" = Or

--data BinOp = Plus | Mult | Minus | Div | Mod | LessEq | Eq | And | Or deriving (Eq, Show)
binOps :: [String]
binOps = ["+","*","-","/","%","<=",">=","<",">","==","!=","&&","||"]

-- example:
-- (x ~~> True
-- ((x) ~~> True
-- ((x  ~~> False
braceToClose :: String -> Bool
braceToClose = (==1) . foldl f 0 where
  f l '(' = l+1
  f l ')' = l-1
  f l _   = l

bracesClosed :: Char -> Char -> String -> Bool
bracesClosed ch1 ch2 = (==0) . foldl f 0 where
  f l ch | ch == ch1 = l+1
  f l ch | ch == ch2 = l-1
  f l _   = l

-- takeUntilBracesClosed "meme{{meow} wo}f" '{' '}' ~~> Just "meme{{meow} wo}"
takeUntilBracesClosed :: String -> Char -> Char -> Maybe String
takeUntilBracesClosed str ch1 ch2 = f str "" 0 True where
  f :: String -> String -> Int -> Bool -> Maybe String
  f _        res i b | i == 0 && not b                            = Just res
  f str      res _ _ | null str && not (bracesClosed ch1 ch2 res) = Nothing
  f str      res _ _ | null str && bracesClosed ch1 ch2 res       = Just res
  f (x:rest) res i _ | x == ch1                                   = f rest (res ++ [x]) (i+1) False
  f (x:rest) res i _ | x == ch2                                   = f rest (res ++ [x]) (i-1) False
  f (x:rest) res i _ | x /= ch1 && x /= ch2                       = f rest (res ++ [x]) i True

removeOuterBraces :: String -> String
removeOuterBraces "" = ""
removeOuterBraces (x:rest) | x /= '(' = x : removeOuterBraces rest
removeOuterBraces al@(x:rest) | x == '(' && not (bracesClosed '(' ')' al) = al
removeOuterBraces al@(x:rest) | x == '(' && bracesClosed '(' ')' al = f 1 rest ""
  where
    f :: Int -> String -> String -> String
    f 0 str        res = init res ++ str
    f i ""         res = res
    f i ('(':rest) res = f (i+1) rest (res ++ "(")
    f i (')':rest) res = f (i-1) rest (res ++ ")")
    f i (x:rest)   res = f i rest (res ++ [x])
