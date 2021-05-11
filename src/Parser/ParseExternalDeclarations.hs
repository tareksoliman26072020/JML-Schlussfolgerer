module Parser.ParseExternalDeclarations where

import Parser.Types
import Parser.Parser
import Parser.PrimitiveFunctionality
import Parser.ParseExpressions
import Parser.ParseStatements

import Control.Applicative
import Control.Monad
import Control.Monad.State(runStateT)
import Data.Char(isAlphaNum,isSpace,isDigit)
import Data.List.Split(splitOn)
import Data.List(isPrefixOf,(\\),elemIndices,isInfixOf)
import Data.Maybe(fromJust,fromMaybe,isJust,isNothing)
import Data.Foldable(asum)

-- ExternalDeclaration =
--   FunDef {
--     funModifier :: [Modifier],
--     funDecl :: Statement,
--     throws :: Maybe Exception,
--     funBody :: Statement}
parseFunDef :: Parser ExternalDeclaration
parseFunDef = do
  maybeModifier <- optional $ parseModifier
  funDecll <- parseFunDecl
  state <- getState'
  let x = takeUntilBracesClosed state '{' '}'
  if isNothing x then failure
  else
    let y = runStateT (parseFunDef' maybeModifier funDecll) (fromJust x)
    in
      if isNothing y then failure
      else do
        newState (state \\ fromJust x) <* whitespace_linebreak
        return $ fst $ fromJust y
  where
    parseFunDef' :: Maybe [Modifier] -> Statement -> Parser ExternalDeclaration
    parseFunDef' maybeModifier funDecll = do
      next <- token "throws" <|> token "{"
      if next == "{" then do
        state <- getState'
        let x = takeUntilBracesClosed ("{"++state) '{' '}'
        if isNothing x then failure
        else do
          newState $ init state
          finish maybeModifier funDecll Nothing
      else do
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
                many (satisfy (\ch -> ch /= ' ' && ch /= '(' && ch /= ';' && ch /= '{'))
        token "{"
        state <- getState'
        newState $ init state
        finish maybeModifier funDecll (Just $ toException excp)

    finish :: Maybe [Modifier] -> Statement -> Maybe Exception -> Parser ExternalDeclaration
    finish maybeModifier funDecll maybeException = do
      body <- parseCompStmt ";"
      return $ FunDef {funModifier = fromMaybe [] maybeModifier,
                       funDecl     = funDecll,
                       throws      = maybeException,
                       funBody     = body}


parseFunDecl :: Parser Statement
parseFunDecl = do
  name <- parseVariableExpr "("
  state <- getState'
  let unrefined = takeUntilBracesClosed state '(' ')'
  if isNothing unrefined then failure
  else do
    let splitted = splitOn "," (tail $ init $ fromJust unrefined)
        listArgs = if length splitted == 1 && length (head splitted) == 0 then []
                   else map (fst . fromJust . (runStateT (parseVariableExpr ";")) . (++";")) splitted
    newState (state \\ fromJust unrefined)
    return $ FunCallStmt $ FunCallExpr {funName = name, funArgs = listArgs}

parseFunDefs :: Parser [ExternalDeclaration]
parseFunDefs = many $ parseFunDef

{-
data ExternalDeclaration = FunDef {funModifier :: [Modifier],
                                   funDecl :: Statement,
                                   throws :: Maybe Exception,
                                   funBody :: Statement} deriving(Eq,Show)
-}
--VarExpr {varType :: Maybe (Type Types), varObj :: [String], varName :: String}
--FunCallStmt {funCall :: Expression}
--FunCallExpr {funName :: Expression, funArgs :: [Expression]}
returnFunction :: String -> Parser String
returnFunction seeked = do
  state <- getState'
  parsed <- optional parseFunDef
  if isNothing parsed then failure
  else do
    let Just (FunDef _ (FunCallStmt (FunCallExpr (VarExpr _ _ funName) _)) _ _) = parsed
    rest <- getState'
    if seeked == funName then
      let fun = fromJust $ takeUntilBracesClosed state '{' '}'
      in return fun
    else returnFunction seeked
