{-# Language LambdaCase,NamedFieldPuns #-}
module JML.RefineParsed where
import Parser.Types
import Prelude hiding(negate)
import Control.Exception(throw)
import Control.Monad.State(runStateT)
import Data.Maybe(isNothing, fromJust, isJust)
import Parser.ParseExternalDeclarations
import Parser.Parser
import Parser.PrimitiveFunctionality
import Data.List.Split(splitOn)
import Text.Printf

{-
data BBool = BTrue | BFalse | BError String deriving(Eq,Show)

bFilter :: (a -> BBool) -> [a] -> [a]
bFilter f [] = []
bFilter f (x:rest)
  | f x == BTrue = x : bFilter f rest
  | otherwise    = bFilter f rest
-}

findParsedFunction :: String -> [ExternalDeclaration] -> Maybe ExternalDeclaration
findParsedFunction str list =
  let results = filter f list
  in if length results > 1 then throw $ NoteExcp "{{findFunction}}: the seeked function has duplicates" else
     if length results == 1 then Just $ head results
     else Nothing
    where
      f :: ExternalDeclaration -> Bool
      f (FunDef _ _ (FunCallStmt (FunCallExpr (VarExpr _ _ funName) _)) _ _) = funName == str
      f (FunDef _ _ FunCallStmt{} _ _) = throw $ NoteExcp "{{findFunction}}: ExternalDeclaration -> FunDef -> FunCallStmt -> FunCallExpr -/> VarExpr"
      f FunDef{} = throw $ NoteExcp "{{findFunction}}: ExternalDeclaration -> FunDef -/> FunCallStmt"
      --uncomment this in case of extending ExternalDeclaration:
--    f _            = throw $ NoteExcp "{{findFunction}}: ExternalDeclaration -/> FunDef"

--FunDef {funModifier :: [Modifier], funDecl :: Statement, throws :: Maybe Exception, funBody :: Statement}
--FunCallExpr {funName :: Expression, funargs :: [Expression]}
--FunCallStmt {funCall :: Expression}
--VarExpr {varType :: Maybe (Type Types), varObj :: [String], varName :: String}
getFunLocalVariables :: ExternalDeclaration -> [String]
getFunLocalVariables (FunDef _ _ (FunCallStmt (FunCallExpr _ funArgs)) _ funBody)
  | null funArgs = getCompStmtLocalVariables funBody
  | otherwise    = case head funArgs of
    VarExpr{} -> map varName funArgs ++ getCompStmtLocalVariables funBody
    _ -> throw $ NoteExcp "{{getFunLocalVariables}}: ExternalDeclaration -> FunDef -> (FunCallStmt (FunCallExpr _ (-/>VarExpr)))"
--uncomment this in case of extending ExternalDeclaration:
--getFunLocalVariables _ = throw $ NoteExcp "{{getFunLocalVariables}}: ExternalDeclaration -/> FunDef"

getUnparsedFunction :: String -> String -> String
getUnparsedFunction methods seeked = fst $ fromJust $ runStateT getIt methods
  where
    getIt :: Parser String
    getIt = do
      state <- getState'
      parsed <- parseFunDef
      rest <- getState'
      let FunDef modifiers _ (FunCallStmt (FunCallExpr (VarExpr _ _ funName) _)) _ _ = parsed
      if seeked == funName then
        let fun                 = fromJust $ takeUntilBracesClosed state '{' '}'
            splittedAtModifiers = splitOn (fromModifiers modifiers) fun
            yes                 = fromModifiers modifiers ++ last splittedAtModifiers
        in return yes
      else getIt

getFunsNames :: String -> [String]
getFunsNames methods =
  let parsed = runStateT parseFunDefs methods
  in if isNothing parsed || (isJust parsed && (not $ null $ snd $ fromJust parsed))
       then throw $ NoteExcp "{{getFunctions}}: parsed == Nothing || snd parsed /= null"
     else
       let extDecls = fst $ fromJust parsed
       in map f extDecls
  where
    f :: ExternalDeclaration -> String
    f (FunDef _ _ (FunCallStmt (FunCallExpr (VarExpr _ _ funName) _)) _ _) = funName
    f _ = throw $ NoteExcp "{{getFunctions -> f}}: not parsed the right way"

--At this point: variables passed to the function are already stored
--getCompStmtLocalVariables prepares to traverse through the function's contents
--getCompStmtLocalVariables works only on CompStmt :: Statement
getCompStmtLocalVariables :: Statement -> [String]
--TODO: Using Text instead of String is more efficient here:
getCompStmtLocalVariables (CompStmt stmtList) = filter (/= "") (map f1 stmtList)
  where
    --The passed Statement is in the function.
    --This will return the name of the statement's local variable
    f1 :: Statement -> String
    f1 (CompStmt _) = throw $ NoteExcp "A Statement in function is of CompStmt. Why?"
    f1 (VarStmt var) = f2 var
    f1 (AssignStmt _ assign) = f2 assign
    f1 _                     = ""

    --From VarStmt and AssignStmt an Expression is presented.
    --We want the variable name presented in this Expression
    --to be added to the list of local variable list
    f2 :: Expression -> String
    f2 (VarExpr varType _ varName) = if isNothing varType then "" else varName
    f2 (ArrayExpr arrName _)       = f2 arrName
    --f2 (BinOpExpr expr1 _ _)       = f2 expr1
    f2 (AssignExpr assEleft _)     = f2 assEleft
    f2 _                           = ""
getCompStmtLocalVariables a                   = throw $ NoteExcp $ printf "{{getFunLocalVariables}}: ExternalDeclaration -> (FunDef _ _ _ (-/>CompStmt)): %s" (show a)

--data BinOp = Plus | Mult | Minus | Div | Mod | Less | LessEq | Greater | GreaterEq | Eq | Neq | And | Or
--this negates an BinOp,UnOp,BoolLiteral expression
negate :: Expression -> Expression
negate (BoolLiteral bool) = BoolLiteral $ not bool
negate a@(BinOpExpr expr1 binOp expr2) | binOp == And || binOp == Or =
  let newBinOp :: BinOp -> BinOp
      newBinOp And = Or
      newBinOp Or  = And
  in negate (BinOpExpr (negate expr1) (newBinOp binOp) (negate expr2))
negate (BinOpExpr expr1 binOp expr2) =
  let newBinOp :: BinOp -> BinOp
      newBinOp Less = GreaterEq
      newBinOp LessEq = Greater
      newBinOp Greater = LessEq
      newBinOp GreaterEq = Less
      newBinOp Eq = Neq
      newBinOp Neq = Eq
      newBinOp s = throw $ NoteExcp $ printf "{{negate}}: invalid newBinOp: %s" (show s)
  in BinOpExpr expr1 (newBinOp binOp) expr2
--UnOpExpr {unOp :: UnOp, expr :: Expression}
negate (UnOpExpr _ expr) = expr
negate expr = throw $ NoteExcp $ printf "{{negate}}: {{invalid Expression}}:\n %s" (show expr)

appendBoolExprRight :: Expression -> Expression -> Expression
--BinOpExpr {expr1 :: Expression, binOp :: BinOp, expr2 :: Expression}
appendBoolExprRight expr1 expr2 = BinOpExpr expr1 And expr2
{-
appendBoolExprRight (BinOpExpr expr1 binOp expr2@(BinOpExpr _ _ _)) expr3 = BinOpExpr expr1 binOp (appendBoolExprRight expr2 expr3)
appendBoolExprRight (BinOpExpr expr1 binOp expr2@(UnOpExpr _ _)) expr3 = BinOpExpr expr1 binOp (appendBoolExprRight expr2 expr3)
appendBoolExprRight (UnOpExpr unOp expr1@(BinOpExpr _ _ _)) expr2 = UnOpExpr unOp (appendBoolExprRight expr1 expr2)
appendBoolExprRight (UnOpExpr unOp expr1@(UnOpExpr _ _)) expr2 = UnOpExpr unOp (appendBoolExprRight expr1 expr2)
appendBoolExprRight expr1 expr2 = BinOpExpr expr1 And expr2
-}
appendBoolExprLeft :: Expression -> Expression -> Expression
appendBoolExprLeft expr (BinOpExpr expr1 binOp expr2) = BinOpExpr (appendBoolExprLeft expr expr1) binOp expr2
appendBoolExprLeft expr1 expr2 = BinOpExpr expr1 And expr2

fromVarExprToString :: [Expression] -> [String]
fromVarExprToString = map f
  where
    f :: Expression -> String
    --VarExpr {varType :: Maybe (Type Types), varObj :: [String], varName :: String}
    f (VarExpr _ _ varName) = varName
    f _                     = throw $ NoteExcp "{{fromVarExprToString -> f}}: -/> VarExpr"

--known is the varName of some VarExpr.
--We want the whole origin Statement
getStmtOfVar :: [Statement] -> String -> (Statement,String)
getStmtOfVar stmts var =
  let b     = takeFirstOccurrence f stmts
      left  = extract b--throw $ NoteExcp $ printf "refined:\n%s" (show stmts)
      right = whatIsThis b
  in (left,right)
  {-let x = dropWhile f1 stmts
  in if null x then throw $ NoteExcp $ "refined:\n" ++ (show $ head stmts)
     else f2-}
{-
  = CompStmt {statements :: [Statement]}
  | VarStmt {var :: Expression}
  | AssignStmt {varModifier :: [Modifier], assign :: Expression}
  | CondStmt {condition :: Expression, siff :: Statement, selsee :: Statement}
  | ForStmt {acc :: Statement, cond :: Expression, step :: Statement, forBody :: Statement}
  | WhileStmt {condition :: Expression, whileBody :: Statement}
  | FunCallStmt {funCall :: Expression}
  | TryCatchStmt {tryBody :: Statement,
                  catchExcp :: Type Exception, catchBody :: Statement,
                  finallyBody :: Statement}
  | ReturnStmt {returnS :: Maybe Expression}
-}
  where
    takeFirstOccurrence :: (Statement -> Bool) -> [Statement] -> Statement
    takeFirstOccurrence f [] = throw $ NoteExcp "(getStmtOfVar -> takeFIrstOccurrence): given list is empty or Statement were not found"
    takeFirstOccurrence f (x:rest) = if f x then x else takeFirstOccurrence f rest

    -- look up the `var` in the given statement.
    -- if found, then return True
    f :: Statement -> Bool
    --AssignExpr {assEleft :: Expression, assEright :: Expression}
    --VarExpr {varType :: Maybe (Type Types), varObj :: [String], varName :: String}
    f (AssignStmt _ (AssignExpr (VarExpr _ _ varName) _)) | varName == var = True
    f AssignStmt{} = False
    f (ForStmt (CompStmt acc) _ _ (CompStmt body)) = any f (acc++body)
    f (WhileStmt _ (CompStmt body)) = any f body
    f (TryCatchStmt body1 _ body2 body3) = any f [body1,body2,body3]
    f CondStmt {siff, selsee} = any f [siff,selsee]
    f (CompStmt list) = any f list
    f ReturnStmt{} = False--throw $ NoteExcp $ printf "193:\n%s" (show a)

    extract :: Statement -> Statement
    extract (CompStmt stmtList) = takeFirstOccurrence f stmtList
    extract a@AssignStmt{} = a
    extract (ForStmt (CompStmt acc) _ _ (CompStmt body)) = extract (CompStmt $ acc ++ body)
    extract (WhileStmt _ body) = extract body
    extract (TryCatchStmt (CompStmt body1) _ (CompStmt body2) (CompStmt body3)) = extract (CompStmt $ body1 ++ body2 ++ body3)
    --CondStmt {condition :: Expression, siff :: Statement, selsee :: Statement}
    extract CondStmt{siff=CompStmt ifs, selsee=CompStmt elses} = extract (CompStmt $ ifs ++ elses)

    whatIsThis :: Statement -> String
    whatIsThis AssignStmt{}   = "Assign"
    whatIsThis ForStmt{}      = "For"
    whatIsThis WhileStmt{}    = "While"
    whatIsThis TryCatchStmt{} = "Try"
    whatIsThis CondStmt{siff, selsee} = "Cond"--throw $ NoteExcp $ printf "208:\n%s" (show a)
