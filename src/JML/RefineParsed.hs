{-# Language LambdaCase,NamedFieldPuns #-}
module JML.RefineParsed where
import Parser.Types
import Prelude hiding(negate)
import Control.Exception(throw)
import Data.Maybe(isNothing, fromJust, isJust,mapMaybe)
import Data.List(foldl')
import Text.Printf

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

getFunLocalVariables :: ExternalDeclaration -> [String]
getFunLocalVariables (FunDef _ _ (FunCallStmt (FunCallExpr _ funArgs)) _ funBody)
  | null funArgs = getCompStmtLocalVariables funBody
  | otherwise    = case head funArgs of
    VarExpr{} -> map varName funArgs ++ getCompStmtLocalVariables funBody
    _ -> throw $ NoteExcp "{{getFunLocalVariables}}: ExternalDeclaration -> FunDef -> (FunCallStmt (FunCallExpr _ (-/>VarExpr)))"
--uncomment this in case of extending ExternalDeclaration:
--getFunLocalVariables _ = throw $ NoteExcp "{{getFunLocalVariables}}: ExternalDeclaration -/> FunDef"

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

getCompStmtLocalVariables' :: [Statement] -> [Expression]
getCompStmtLocalVariables' = mapMaybe f1
  where
    --The passed Statement is in the function.
    --This will return the name of the statement's local variable
    f1 :: Statement -> Maybe Expression
    f1 (AssignStmt _ assign) = Just assign
    f1 _                     = Nothing

-- extracts mentioned global variables in a list of statements given in a CompStmt
-- parameters and local variables will be excluded
-- variables which are assigned more than once will be excluded as well, because they could be mistaken with global variables.
-- And because a global variables can be re-assigned in the same context, duplicates may occur which are to be eliminated.
-- The output is exclusively of AssignExpr.
getGlobalVariables :: [Statement] -> [Expression] -> [Expression]
getGlobalVariables stmts params =
  let assigns = getCompStmtLocalVariables' stmts -- no variable is mentioned twice
      combined_filtered = filter (\case
        a@AssignExpr{assEleft=b@VarExpr{}} | not (any (\case
          c@VarExpr{}    -> isJust (varType c) && varName c == varName b
          c@AssignExpr{} -> isJust (varType $ assEleft c) && varName (assEleft c) == varName b) (assigns ++ params))
                                                  -> isNothing (varType b)
        _                                         -> False) (assigns ++ params)
      removeDuplicates = foldr (\r l ->
        let var_name = (varName $ assEleft r)
        in if any (\ass -> varName (assEleft ass) == var_name) l
             then l
           else r:l) [] combined_filtered
  in removeDuplicates

getAllGlobalVariable :: ExternalDeclaration -> [[Expression]]
getAllGlobalVariable extDecl@FunDef{} =
  let params = funArgs $ funCall $ funDecl extDecl
      prime  = head $ f params (funBody extDecl)
      allButMain = concatMap (f params) (statements $ funBody extDecl)
  in  if null allButMain then [prime]
      else map (prime ++) allButMain
  where
    f :: [Expression] -> Statement -> [[Expression]]
    f params a@CompStmt{} = [getGlobalVariables (statements a) params]
    f _ VarStmt{} = []
    f _ AssignStmt{} = []
    f params a@CondStmt{} = getGlobalVariables (statements $ siff a) params :
                            getGlobalVariables (statements $ selsee a) params : []
    f params a@ForStmt{} = [getGlobalVariables (statements $ forBody a) params]
    f params a@WhileStmt{} = [getGlobalVariables (statements $ whileBody a) params]
    f _ a@FunCallStmt {} = []
    f params a@TryCatchStmt{} = getGlobalVariables (statements $ tryBody a) params :
                                getGlobalVariables (statements $ catchBody a) params :
                                getGlobalVariables (statements $ finallyBody a) params : []
    f _ a@ReturnStmt{} = []

-- does an unconventional thing of finding global variables, and giving them a type "Global"
-- in order for the functionality in ToJML.hs to acknowledge their existence.
-- This is being done this way, because batching/refactoring ToJML.hs in a way which allows
-- the recognition of global variables has been unsuccessful.
-- That's why the goal here is to make global variables not more global, being giving them a type.
-- After applying this function and in case global variables comes up while jmlifing:
--   new local variables will come into being, which are of type (Class),
--   and they'll be assigned with "this." followd with the name of this variable.
{-
example:
public int foo(){
  z = 0;
  return 9;
}

becomes:

public int foo(){
  Class z = 0;
  return 9;
}
-}
highlightGlobalVariables :: ExternalDeclaration -> ExternalDeclaration
highlightGlobalVariables extDecl =
  let stmts = statements $ funBody extDecl
      args = funArgs $ funCall $ funDecl extDecl
      localV = getCompStmtLocalVariables'' {- (map varName args) -} stmts
      highlightGlobalV = highlightGlobalVariables' stmts (map varName args) localV
  in FunDef{funModifier = funModifier extDecl,
            isPureFlag  = isPureFlag extDecl,
            funDecl     = funDecl extDecl,
            throws      = throws extDecl,
            funBody     = CompStmt highlightGlobalV}
  where
    getCompStmtLocalVariables'' :: [Statement] -> [String]
    getCompStmtLocalVariables'' = foldl' f1 []

    highlightGlobalVariables' :: [Statement] -> [String] -> [String] -> [Statement]
    highlightGlobalVariables' stmts args localV = concatMap (f2 args localV) stmts

    highlightRight :: [String] -> [String] -> Expression -> [Statement]
    highlightRight args localV expr = case expr of
      IntLiteral _ -> []
      BoolLiteral _ -> []
      CharLiteral _ -> []
      StringLiteral _ -> []
      Null -> []
      b@VarExpr{} | all (varName b `notElem`) [args,localV] -> [AssignStmt{varModifier = [],
                                                                           assign = AssignExpr{assEleft = VarExpr {varType = Just (AnyType{typee="Class",generic=Nothing}),
                                                                                                                   varObj  = varObj b,
                                                                                                                   varName = varName b},
                                                                                               assEright = StringLiteral ("this." ++ varName b)}}]
      b@BinOpExpr{} -> highlightRight args localV (expr1 b) ++ highlightRight args localV (expr2 b)
      UnOpExpr{expr=expr'}  -> highlightRight args localV expr'
      CondExpr{} -> undefined
      _ -> []
    -- is meant as high-order function for getting local variables
    f1 :: [String] -> Statement -> [String]
    f1 acc a@VarStmt{} | isJust $ varType $ var a{- && (varName $ var a) `notElem` args-} = acc ++ [varName $ var a]
    f1 acc a@AssignStmt{} = f1 acc (VarStmt (assEleft $ assign a))
    f1 acc _ = acc


    f2 :: [String] -> [String] -> Statement -> [Statement]
    f2 args localV a@VarStmt{}
      | all ((varName $ var a) `notElem`) [args,localV] = [VarStmt{var     = VarExpr {varType=Just (AnyType{typee="Class",generic=Nothing}),
                                                                   varObj  = varObj (var a),
                                                                   varName = varName (var a)}}]
    f2 args localV a@CompStmt{} = [CompStmt{statements = highlightGlobalVariables' (statements a) args (localV ++ getCompStmtLocalVariables'' (statements a))}]
    f2 args localV a@AssignStmt{} =
      highlightRight args localV (assEright $ assign a)
        ++ [AssignStmt{varModifier = varModifier a,
                       assign      = AssignExpr{assEleft  = var $ head (f2 args localV (VarStmt $ assEleft $ assign a)),
                                                assEright = assEright $ assign a}}]
    f2 args localV a@CondStmt{} = highlightRight args localV (condition a) ++
                                  [CondStmt{condition = condition a,
                                            siff = head $ f2 args localV (siff a),
                                            selsee = head $ f2 args localV (selsee a)}]
    f2 _ _ a = [a]

--this negates an BinOp,UnOp,BoolLiteral expression
negateBoolLiteral :: Expression -> Expression
negateBoolLiteral (BoolLiteral bool) = BoolLiteral $ not bool
negateBoolLiteral (BinOpExpr expr1 binOp expr2) | binOp == And || binOp == Or =
  let newBinOp :: BinOp -> BinOp
      newBinOp And = Or
      newBinOp Or  = And
  in negateBoolLiteral (BinOpExpr (negateBoolLiteral expr1) (newBinOp binOp) (negateBoolLiteral expr2))
negateBoolLiteral (BinOpExpr expr1 binOp expr2) =
  let newBinOp :: BinOp -> BinOp
      newBinOp Less = GreaterEq
      newBinOp LessEq = Greater
      newBinOp Greater = LessEq
      newBinOp GreaterEq = Less
      newBinOp Eq = Neq
      newBinOp Neq = Eq
      newBinOp s = throw $ NoteExcp $ printf "{{negateBoolLiteral}}: invalid newBinOp: %s" (show s)
  in BinOpExpr expr1 (newBinOp binOp) expr2
negateBoolLiteral (UnOpExpr _ expr) = expr
negateBoolLiteral expr = throw $ NoteExcp $ printf "{{negateBoolLiteral}}: {{invalid Expression}}:\n %s" (show expr)

appendBoolExprRight :: Expression -> Expression -> Expression
appendBoolExprRight = (`BinOpExpr` And)

appendBoolExprLeft :: Expression -> Expression -> Expression
appendBoolExprLeft expr (BinOpExpr expr1 binOp expr2) = BinOpExpr (appendBoolExprLeft expr expr1) binOp expr2
appendBoolExprLeft expr1 expr2 = BinOpExpr expr1 And expr2

fromVarExprToString :: [Expression] -> [String]
fromVarExprToString = map f
  where
    f :: Expression -> String
    f (VarExpr _ _ varName) = varName
    f _                     = throw $ NoteExcp "{{fromVarExprToString -> f}}: -/> VarExpr"

--known is the varName of some VarExpr.
--We want the whole origin Statement
getStmtOfVar :: [Statement] -> String -> (Statement,String)
getStmtOfVar stmts var =
  let b     = takeFirstOccurrence f stmts
      left  = extract b
      right = whatIsThis b
  in (left,right)
  where
    takeFirstOccurrence :: (Statement -> Bool) -> [Statement] -> Statement
    takeFirstOccurrence f [] = throw $ NoteExcp "(getStmtOfVar -> takeFirstOccurrence): given list is empty or Statement were not found"
    takeFirstOccurrence f (x:rest) = if f x then x else takeFirstOccurrence f rest

    -- look up the `var` in the given statement.
    -- if found, then return True
    f :: Statement -> Bool
    f (AssignStmt _ (AssignExpr (VarExpr _ _ varName) _)) | varName == var = True
    f AssignStmt{} = False
    f (ForStmt assignStmt _ _ (CompStmt body)) = any f (assignStmt : body)
    f (WhileStmt _ (CompStmt body)) = any f body
    f (TryCatchStmt body1 _ body2 body3) = any f [body1,body2,body3]
    f CondStmt {siff, selsee} = any f [siff,selsee]
    f (CompStmt list) = any f list
    f ReturnStmt{} = False

    extract :: Statement -> Statement
    extract (CompStmt stmtList) = takeFirstOccurrence f stmtList
    extract a@AssignStmt{} = a
    extract (ForStmt assignStmt _ _ (CompStmt body)) = extract (CompStmt $ assignStmt : body)
    extract (WhileStmt _ body) = extract body
    extract (TryCatchStmt (CompStmt body1) _ (CompStmt body2) (CompStmt body3)) = extract (CompStmt $ body1 ++ body2 ++ body3)
    extract CondStmt{siff=CompStmt ifs, selsee=CompStmt elses} = extract (CompStmt $ ifs ++ elses)

    whatIsThis :: Statement -> String
    whatIsThis AssignStmt{}   = "Assign"
    whatIsThis ForStmt{}      = "For"
    whatIsThis WhileStmt{}    = "While"
    whatIsThis TryCatchStmt{} = "Try"
    whatIsThis CondStmt{siff, selsee} = "Cond"

-- copulate each parameter with its actual value
-- in case the function with the passed name was internally called
copulateVarNameToActualParameters :: [ExternalDeclaration] -> String -> [Expression] -> [Expression]
copulateVarNameToActualParameters extDeclList funName actParExprList =
  let funFound = filter (\case FunDef{funDecl=FunCallStmt{funCall=FunCallExpr{funName=VarExpr{varName}}}} -> varName == funName) extDeclList
  in if null funFound then throw $ NoteExcp $ printf "{{ToJML -> attachVarNameToActualParameters}}: searched function name wasn't found" funName
     else
       let FunDef{funDecl=FunCallStmt{funCall=FunCallExpr{funArgs}}} = head funFound
       in zipWith (\l r -> AssignExpr{assEleft=l,assEright=r}) funArgs actParExprList