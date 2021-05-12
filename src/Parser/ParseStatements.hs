module Parser.ParseStatements where

import Parser.Types
import Parser.Parser
import Parser.PrimitiveFunctionality
import Parser.ParseExpressions

import Control.Applicative
import Control.Monad
import Control.Monad.State(runStateT)
import Data.Char(isAlphaNum,isSpace,isDigit)
import Data.List.Split(splitOn)
import Data.List(isPrefixOf,(\\),elemIndices,isInfixOf,foldl')
import Data.Maybe(fromJust,fromMaybe,isJust,isNothing)
import Data.Foldable(asum)

parseCompStmt :: String -> Parser Statement
parseCompStmt strr = do
  whitespace_linebreak_semiColon
  state <- getState'
  if null state then return $ CompStmt []
  else do
    stmt <- parseWhile <|> parseFor <|> parseCondStmt <|> (parseFunCallStmt ";") <|> parseTryCatch <|> parseReturnStmt <|> parseAssignStmt <|> parseVariableStmt strr
    optional (token strr)
    rec <- optional(parseCompStmt strr)
    if isNothing rec then return $ CompStmt [stmt]
    else
      let CompStmt list = fromJust rec
      in return $ CompStmt $ stmt : list
      --return $ CompStmt $ stmt : [fromJust rec]

parseVariableStmt :: String -> Parser Statement
parseVariableStmt strr = VarStmt <$> parseVariableExpr strr

parseAssignStmt :: Parser Statement
parseAssignStmt = liftM2
  (\e1 e2 ->
    AssignStmt {varModifier = fromMaybe [] e1, assign = e2})
      (optional $ parseModifier) $ parseAssignExpr

-- CondStmt {condition :: Expression, siff :: Statement, selsee :: Statement}
parseCondStmt :: Parser Statement
parseCondStmt = do
  token "if"
  token "("
  cond <- parseBinOp ")" <|> parseUnOp ")" <|> parseBool <|> (parseFunCallExpr ")")
  token ")"
  state1 <- getState'
  let maybeState1 = takeUntilBracesClosed state1 '{' '}'
  if isNothing maybeState1 then failure
  else do
    let ifBody = init $ tail $ fromJust maybeState1
        ifBodyParsed = runStateT (parseCompStmt ";") ifBody
    newState (state1 \\ fromJust maybeState1)
    token "else"
    state2 <- getState'
    let maybeState2 = takeUntilBracesClosed state2 '{' '}'
    if isNothing maybeState2 then failure
    else do
      let elseBody = init $ tail $ fromJust maybeState2
          elseBodyParsed = runStateT (parseCompStmt ";") elseBody
      newState (state2 \\ fromJust maybeState2)
      return $ CondStmt {condition = cond,
                         siff = fst (fromMaybe (CompStmt [],"") ifBodyParsed),
                         selsee = fst (fromMaybe (CompStmt [],"") elseBodyParsed)}

parseFunCallStmt :: String -> Parser Statement
parseFunCallStmt = liftM FunCallStmt . parseFunCallExpr

-- ForStmt {acc :: Statement, cond :: Expression, step :: Statement, forBody :: Statement}
parseFor :: Parser Statement
parseFor = do
  token "for"
  state <- takeUntilChar '{'
  guard(bracesClosed '(' ')' state)
  let forDeclList = splitOn ";" $ init $ tail state
      accs  = fst $ fromJust $ runStateT (parseCompStmt ";") (refine (forDeclList !! 0) ++ ";")
      condd = fst $ fromJust $ runStateT (parseBinOp ";") (forDeclList !! 1 ++ ";")
      steps = fst $ fromJust $ runStateT (parseCompStmt ";") (refine (forDeclList !! 2) ++ ";")
  state2 <- getState'
  let body = takeUntilBracesClosed state2 '{' '}'
  if isNothing body then failure
  else do
    newState $ init $ tail (fromJust body)
    body2 <- parseCompStmt ";"
    newState $ state2 \\ (fromJust body)
    return $ ForStmt{acc  = accs,
                     cond = condd,
                     step = steps,
                     forBody = body2}
  where
    refine :: String -> String
    refine str = (foldl' (\l r -> if r==',' then l++";" else l++[r]) "" str)


-- WhileStmt {condition :: Expression, whileBody :: Statement}
parseWhile :: Parser Statement
parseWhile = do
  token "while"
  state <- takeUntilChar '{'
  guard(bracesClosed '(' ')' state)
  let cond = fst $ fromJust $ runStateT (parseBinOp ";" <|> parseBool <|> parseUnOp ";") $ (tail $ init $ state) ++ ";"
  token "{"
  body <- parseCompStmt ";"
  token "}"
  return $ WhileStmt {condition = cond, whileBody = body}

-- TryCatchStmt {tryBody :: Statement, catchExcp :: Type Exception, finallyBody :: Statement}
parseTryCatch :: Parser Statement
parseTryCatch = do
  ------------------------------try------------------------------
  token "try"
  state1 <- getState'
  let tryBody = takeUntilBracesClosed state1 '{' '}'
  if isNothing tryBody then failure
  else do
    newState (state1 \\ fromJust tryBody)
    ------------------------------catch------------------------------
    token "catch"
    state2 <- getState'
    let catchCond = takeUntilBracesClosed state2 '(' ')'
    if isNothing catchCond then failure
    else do
      newState (state2 \\ fromJust catchCond)
      whitespace_linebreak
      state3 <- getState'
      let catchBody = takeUntilBracesClosed state3 '{' '}'
      if isNothing catchBody then failure
      else do
        newState (state3 \\ fromJust catchBody)
        ------------------------------finally------------------------------
        finallyMaybe <- optional $ token "finally"
        if isNothing finallyMaybe then
          let x1 = f1 (core tryBody) (core catchCond) (core catchBody)
          in if isNothing x1 then failure
             else return $ fromJust x1
        else do
          whitespace_linebreak
          state4 <- getState'
          let finallyBody = takeUntilBracesClosed state4 '{' '}'
          if isNothing finallyBody then failure
          else
            let x2 = f2 (core tryBody) (core catchCond) (core catchBody) (core finallyBody)
            in if isNothing x2 then failure
               else newState (state4 \\ fromJust finallyBody) *> (return $ fromJust x2)
  where
    f1 :: String -> String -> String -> Maybe Statement
    f1 tryy catch1 catch2 =
      let x1 = runStateT (parseCompStmt ";") tryy
          x2 = f3
          x3 = runStateT (parseCompStmt ";") catch2
          no1 = isNothing x3 && null catch2
          no2 = isNothing x3 && not (null catch2)
          empty = CompStmt []
          cb = if no1 then empty else fst $ fromJust x3
      in if no2 && (any isNothing [x1,x3] || isNothing (runStateT x2 catch2)) then Nothing
         else Just $ TryCatchStmt
           {tryBody = fst $ fromJust x1,
            catchExcp = fst $ fromJust $ runStateT x2 catch1,
            catchBody = cb,
            finallyBody = empty}

    f2 :: String -> String -> String -> String -> Maybe Statement
    f2 tryy catch1 catch2 finally =
      let x1 = runStateT (parseCompStmt ";") tryy
          x2 = f3
          x3 = runStateT (parseCompStmt ";") catch2
          x4 = runStateT (parseCompStmt ";") finally
          no1_1 = isNothing x3 && null catch2
          no1_2 = isNothing x3 && not (null catch2)
          no2_1 = isNothing x4 && null finally
          no2_2 = isNothing x4 && not (null finally)
          empty = CompStmt []
          cb = if no1_1 then empty else fst $ fromJust x3
          fb = if no2_1 then empty else fst $ fromJust x4
      in if no1_2 && no2_2 &&
            (any isNothing [x1,x3,x4] || isNothing (runStateT x2 catch2)) then Nothing
         else
           Just $ TryCatchStmt
             {tryBody = fst $ fromJust x1,
              catchExcp = fst $ fromJust $ runStateT x2 catch1,
              catchBody = cb,
              finallyBody = fb}

    core :: Maybe [a] -> [a]
    core = tail . init . fromJust

    f3 :: Parser (Type Exception)
    f3 = do
      excp <- optional (
                token "Exception"                       <|>
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
                token "IllegalArgumentException")
      if isNothing excp then
        fmap (\l -> AnyType {typee = l, generic = Nothing})
          (many $ satisfy (not . isSpace))
      else return $ BuiltInType $ toException $ fromJust excp

-- TryCatchStmt {tryBody :: Statement, catchExcp :: Type Exception, finallyBody :: Statement}
-- ReturnStmt {returnS :: Maybe Expression}
parseReturnStmt :: Parser Statement
parseReturnStmt = (do
  token "return"
  maybeDone <- optional $ token ";"
  if isJust maybeDone then return $ ReturnStmt Nothing
  else do
    res <- parseBinOp ";" <|> parseUnOp ";" <|> parseArray <|> parseFunCallExpr ";" <|>
           parseInt <|> parseChar <|> parseString <|> parseBool <|> parseNull <|> parseVariableExpr ";"
    optional $ token ";"
    return $ ReturnStmt (Just res)) <|> fmap (ReturnStmt . Just) parseExcp
