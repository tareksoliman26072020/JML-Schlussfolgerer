{-# Language LambdaCase,NamedFieldPuns #-}
module JML.RefineParsed where
import Parser.Types
import Prelude hiding(negate)
import Control.Exception(throw)
import Data.Maybe(isNothing, fromJust, isJust,mapMaybe)
import Data.List(intercalate,foldl')
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
  in removeDuplicates --throw $ NoteExcp $ printf $ intercalate "\n" $ map show removeDuplicates

getAllGlobalVariable :: ExternalDeclaration -> [[Expression]]
getAllGlobalVariable extDecl@FunDef{} =
  let params = funArgs $ funCall $ funDecl extDecl
      prime  = head $ f params (funBody extDecl)
      allButMain = concatMap (f params) (statements $ funBody extDecl)
  in  if null allButMain then [prime]
      else map (prime ++) allButMain
  where
    f :: [Expression] -> Statement -> [[Expression]]
    --CompStmt {statements :: [Statement]}
    f params a@CompStmt{} = [getGlobalVariables (statements a) params]
    f _ VarStmt{} = []
    f _ AssignStmt{} = []
    --CondStmt {condition :: Expression, siff :: Statement, selsee :: Statement}
    f params a@CondStmt{} = getGlobalVariables (statements $ siff a) params :
                            getGlobalVariables (statements $ selsee a) params : []
    --ForStmt {acc :: Statement, cond :: Expression, step :: Statement, forBody :: Statement}
    f params a@ForStmt{} = [getGlobalVariables (statements $ forBody a) params]
    --WhileStmt {condition :: Expression, whileBody :: Statement}
    f params a@WhileStmt{} = [getGlobalVariables (statements $ whileBody a) params]
    f _ a@FunCallStmt {} = []
    {-TryCatchStmt {tryBody :: Statement,
                        catchExcp :: Type Exception, catchBody :: Statement,
                        finallyBody :: Statement}-}
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
  Global z = 0;
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
    highlightRight args localV expr = case expr{-assEright $ assign a-} of
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
      b@UnOpExpr{expr=expr'}  -> highlightRight args localV expr'
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
    --CondStmt {condition :: Expression, siff :: Statement, selsee :: Statement}
    f2 args localV a@CondStmt{} = highlightRight args localV (condition a) ++
                                  [CondStmt{condition = condition a,
                                            siff = head $ f2 args localV (siff a),
                                            selsee = head $ f2 args localV (selsee a)}]
    f2 _ _ a = [a]

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
negate (UnOpExpr _ expr) = expr
negate expr = throw $ NoteExcp $ printf "{{negate}}: {{invalid Expression}}:\n %s" (show expr)

appendBoolExprRight :: Expression -> Expression -> Expression
appendBoolExprRight expr1 expr2 = BinOpExpr expr1 And expr2

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
  where
    takeFirstOccurrence :: (Statement -> Bool) -> [Statement] -> Statement
    takeFirstOccurrence f [] = throw $ NoteExcp "(getStmtOfVar -> takeFIrstOccurrence): given list is empty or Statement were not found"
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

-- looks up the function whose name is passed to this function.
-- After funding the function in the list of external declarations, this function will be altered in a way
-- that expressions which are not meant to be processed will be cut short.
-- Only boolean expressions in if statements are being minded here.
-- Boolean expressions of while- and for- statements are not being minded.
{-
example:

int foo(int i){
  if(i>=0) return i;
  else return (-1)*i;
}

so when foo(5):
int foo(int i){
  if(i>=0) return i;
}

and when foo(-5):
int foo(int i){
  if(i<0) return (-1)*i;
}
-}
enforceActualParameterEvaluation :: [ExternalDeclaration] -> String -> [Expression] -> [ExternalDeclaration]
enforceActualParameterEvaluation extDeclList funName params = map f1 extDeclList
  where
    -- is meant as a high-order function,
    -- and helps looking up the function whose name was passed to this function,
    -- and solely altering it
    f1 :: ExternalDeclaration -> ExternalDeclaration
    f1 a@FunDef{funDecl=FunCallStmt{funCall=b@FunCallExpr{funName=VarExpr{varName}}},funBody=CompStmt{statements}}
      | varName == funName =
          let funAltered = enforceEvaluation statements (params ++ getCompStmtLocalVariables' statements)
          in FunDef{funModifier = funModifier a,
                    isPureFlag  = isPureFlag a,
                    funDecl = FunCallStmt{funCall=b},
                    throws = throws a,
                    funBody = CompStmt{statements=funAltered}}
    f1 extDecl = extDecl

-- resembles `enforceActualParameterEvaluation`.
-- Difference is: While `enforceActualParameterEvaluation` minds the actual parameters and the function's local variables,
-- it minds only local functions
enforceLocalVariablesEvaluation :: ExternalDeclaration -> ExternalDeclaration
enforceLocalVariablesEvaluation a@FunDef{funBody=CompStmt statements} =
  FunDef{funModifier = funModifier a,
         isPureFlag = isPureFlag a,
         funDecl = funDecl a,
         throws = throws a,
         funBody = CompStmt (enforceEvaluation statements (getCompStmtLocalVariables' statements))}

-- is meant to be use in `enforceActualParameterEvaluation`, `enforceLocalVariablesEvaluation`
-- takes the actual parameters/local variables into consideration to alter the function's statements
enforceEvaluation :: [Statement] -> [Expression] -> [Statement]
enforceEvaluation stmts localVars = map (`f2` localVars) stmts
  where
    -- is meant as a high-order function,
    -- and helps looking up the addressed if-else statements and altering its body
    -- Due to the fact, the other nested statements could, be found, then this will be also examined
    f2 :: Statement -> [Expression] -> Statement
    f2 a@CondStmt{} localVars
      | isJust (lookUpVar' a localVars) &&
        cannotEvaluate (condition a) (fromJust $ lookUpVar' a localVars) =
          let newCondition = insertActualParameter (condition a) localVars
              CompStmt{statements = theIfs} = siff a
              CompStmt{statements = theElses} = selsee a
          in CondStmt {condition = newCondition,
                       siff      = CompStmt{statements = enforceEvaluation theIfs (localVars ++ getCompStmtLocalVariables' theIfs)},
                       selsee    = CompStmt{statements = enforceEvaluation theElses (localVars ++ getCompStmtLocalVariables' theElses)}}
      -- delete if
      | fst (evaluate_if_else (condition a) localVars) &&
        fst (snd $ evaluate_if_else (condition a) localVars) =
          let CompStmt{statements = theElses} = selsee a
          in CondStmt{condition = BoolLiteral False,
                      siff = CompStmt{statements=[]},
                      selsee = CompStmt{statements = enforceEvaluation theElses (localVars ++ getCompStmtLocalVariables' theElses)}}
      -- delete else
      | fst (evaluate_if_else (condition a) localVars) &&
        snd (snd $ evaluate_if_else (condition a) localVars) =
          let CompStmt{statements = theIfs} = siff a
          in CondStmt{condition = BoolLiteral True,
                      siff = CompStmt{statements = enforceEvaluation theIfs (localVars ++ getCompStmtLocalVariables' theIfs)},
                      selsee = CompStmt{statements=[]}}
      -- alter body of if and else in case the seeked CondStmt is nested
      | not (fst $ evaluate_if_else (condition a) localVars) =
          let CompStmt{statements = theIfs}   = siff a
              CompStmt{statements = theElses} = selsee a
          in CondStmt{condition = condition a,
                      siff      = CompStmt{statements = enforceEvaluation theIfs (localVars ++ getCompStmtLocalVariables' theIfs)},
                      selsee    = CompStmt{statements = enforceEvaluation theElses (localVars ++ getCompStmtLocalVariables' theElses)}}
    f2 a@ForStmt{} localVars =
      let CompStmt{statements = orgBody} = forBody a
      in ForStmt{acc = acc a,
                 cond = cond a,
                 step = step a,
                 forBody = CompStmt{statements = enforceEvaluation orgBody (localVars ++ getCompStmtLocalVariables' orgBody)}}
    f2 a@WhileStmt{} localVars =
      let CompStmt{statements = orgBody} = whileBody a
      in WhileStmt{condition = insertActualParameter (condition a) localVars,
                   whileBody = CompStmt{statements = enforceEvaluation orgBody (localVars ++ getCompStmtLocalVariables' orgBody)}}
    f2 a@TryCatchStmt{} localVars =
      let CompStmt{statements = tryOrgBody} = tryBody a
          CompStmt{statements = catchOrgBody} = catchBody a
          CompStmt{statements = finallyOrgBody} = finallyBody a
      in TryCatchStmt{tryBody = CompStmt{statements = enforceEvaluation tryOrgBody (localVars ++ getCompStmtLocalVariables' tryOrgBody)},
                      catchExcp = catchExcp a,
                      catchBody = CompStmt{statements = enforceEvaluation catchOrgBody (localVars ++ getCompStmtLocalVariables' catchOrgBody)},
                      finallyBody = CompStmt{statements = enforceEvaluation finallyOrgBody (localVars ++ getCompStmtLocalVariables' finallyOrgBody)}}
    f2 ReturnStmt{returnS = Just a} localVars = ReturnStmt{returnS = Just $ insertActualParameter a localVars} --throw $ NoteExcp $ printf "\n___\n%s\n___\n" (show $ insertActualParameter a localVars)
    f2 stmt localVars = stmt

    -- it examines the if-else statement, to know whether it's the one whose body should be altered.
    -- If it is the one, then (fst evaluate_if_else) == True. Otherwise its return type is (False,undefined)
    -- Also: If it is the one, and the if statement needs to altered,   then its return type is (True,(True,False))
    --       If it is the one, and the else statement needs to altered, then its return type is (True,(False,True))
    -- evaluate_if_else accepts as an Expression only BinOpExpr, UnOpExpr, BoolLiteral
    evaluate_if_else :: Expression -> [Expression] -> (Bool,(Bool,Bool))
    evaluate_if_else a@BinOpExpr{expr1, binOp, expr2} localVars =
      let searching = lookUpVar expr1 expr2 localVars
      in
         maybe (False,undefined) (\case
        1 -> if boolBinOpEvaluate localVars (insertActualParameter expr1 localVars) binOp expr2
               then (True,(False,True))
             else (True,(True,False))
        2 -> if boolBinOpEvaluate localVars expr1 binOp (insertActualParameter expr2 localVars)
               then (True,(False,True))
             else (True,(True,False))
        3 -> if boolBinOpEvaluate localVars (insertActualParameter expr1 localVars) binOp (insertActualParameter expr2 localVars)
               then (True,(False,True))
             else (True,(True,False))) searching
    evaluate_if_else UnOpExpr{unOp, expr} localVars = maybe (False,undefined) (\case
      1 -> if boolUnOpEvaluate localVars (insertActualParameter expr localVars)
             then (True,(False,True))
           else (True,(True,False))) (lookUpVar'' expr localVars)
    evaluate_if_else (BoolLiteral True) _ = (True,(False,True))
    evaluate_if_else (BoolLiteral False) _ = (True,(True,False))
    evaluate_if_else a _ = throw $ NoteExcp $ printf "\n___\n{{RefineParsed.hs -> enforceLocalVariablesEvaluation -> evaluate_if_else}}: the passed Expression isn't right:\n%s\n___\n" (show a)

    cannotEvaluate :: Expression -> Int -> Bool
    cannotEvaluate a@BinOpExpr{} expr_pos = case expr_pos of
      1 -> case expr2 a of
             IntLiteral _ -> False
             BoolLiteral _ -> False
             StringLiteral _ -> False
             CharLiteral _ -> False
             _ -> True
      2 -> case expr1 a of
             IntLiteral _ -> False
             BoolLiteral _ -> False
             StringLiteral _ -> False
             CharLiteral _ -> False
             _ -> True
    cannotEvaluate a@UnOpExpr{} _ = case expr a of
      IntLiteral _ -> False
      BoolLiteral _ -> False
      StringLiteral _ -> False
      CharLiteral _ -> False
      _ -> True

    -- looks up the arguments in the given list of parameters/local variables
    -- If the two passed expressions are uneffected by this list, then its return is Nothing
    -- If the first passed argument has a parameter, then its return is Just 1
    -- If the second passed argument has a parameter, then its return is Just 2
    -- If both Expressions have parameters, then its return is Just 3
    lookUpVar :: Expression -> Expression -> [Expression] -> Maybe Int
    lookUpVar expr1 expr2 localVars = case map (`hasParam` localVars) [expr1,expr2] of
      [True,True] -> Just 3
      [True,False] -> Just 1
      [False,True] -> Just 2
      [False,False] -> Nothing

    -- is similar to lookUpVar, but meant to deal with UnOpExpr instead of BinOpExpr
    lookUpVar'' :: Expression -> [Expression] -> Maybe Int
    lookUpVar'' expr localVars | expr `hasParam` localVars = Just 1
    lookUpVar'' expr _ = Nothing

    -- is similar to lookUpVar
    -- Difference ist: it expects a CondStmt instead of two expressions
    lookUpVar' :: Statement -> [Expression] -> Maybe Int
    lookUpVar' a@CondStmt{condition=BoolLiteral _} localVars = Nothing
    lookUpVar' a@CondStmt{condition=b@BinOpExpr{}} localVars = lookUpVar (expr1 b) (expr2 b) localVars
    lookUpVar' a@CondStmt{condition=b@UnOpExpr{}} localVars  = lookUpVar'' (expr b) localVars

    -- checks whether an Expression is mentioned in the list of actual parameters/local variables.
    -- It works recursively.
    hasParam :: Expression -> [Expression] -> Bool
    hasParam (IntLiteral _)  _ = False
    hasParam (BoolLiteral _) _ = False
    hasParam (CharLiteral _) _ = False
    hasParam (StringLiteral _) _ = False
    hasParam Null _ = False
    hasParam a@VarExpr{} localVars = any (\case AssignExpr{assEleft} -> varName a == varName assEleft) localVars
    hasParam ArrayExpr{arrName} localVars = hasParam arrName localVars
    hasParam BinOpExpr{expr1,expr2} localVars = any (`hasParam` localVars) [expr1,expr2]
    hasParam UnOpExpr{expr} localVars = hasParam expr localVars
    hasParam FunCallExpr{} _ = False
    hasParam CondExpr{eiff,ethenn,eelsee} localVars = any (`hasParam` localVars) [eiff,ethenn,eelsee]
    hasParam AssignExpr{assEleft,assEright} localVars = any (`hasParam` localVars) [assEleft,assEright]
    hasParam ExcpExpr{} _ = throw $ NoteExcp "\n___\n{{RefineParsed.hs -> enforceEvaluation -> hasParam -> ExcpExpr}}: Unimplemented\n___\n"
    hasParam ReturnExpr{returnE = Nothing} _ = False
    hasParam ReturnExpr{returnE = Just a} localVars = hasParam a localVars

    -- replaces VarExpr with the correspondent literal taken from the list of actual parameters/local variables
    insertActualParameter :: Expression -> [Expression] -> Expression
    insertActualParameter a@VarExpr{} localVars =
      let filtered = filter (\case
            AssignExpr{assEleft=b@VarExpr{}} -> varName a == varName b) localVars
      in if null filtered then a
         else assEright (head filtered)
    insertActualParameter a@BinOpExpr{} localVars =
      let a2 = BinOpExpr{expr1 = insertActualParameter (expr1 a) localVars,
                         binOp = binOp a,
                         expr2 = insertActualParameter (expr2 a) localVars}
          lookedUp = lookUpVar (expr1 a2) (expr2 a2) localVars
      in if isJust lookedUp || all isLiteral [expr1 a2, expr2 a2]
           then evaluate' (expr1 a2) (binOp a2) (expr2 a2)
         else a2--throw $ NoteExcp $ printf "\n___\n%s\n___\n" (show $ evaluate' (expr1 a2) (binOp a2) (expr2 a2))
    insertActualParameter a@UnOpExpr{} localVars =
      let inserted = insertActualParameter (expr a) localVars
          lookedUp = lookUpVar'' inserted localVars
      in if isJust lookedUp
           then evaluate inserted localVars
         else inserted
    insertActualParameter a@FunCallExpr{} localVar =
      let list = map (`insertActualParameter` localVars) (funArgs a)
      in FunCallExpr {funName = funName a,
                      funArgs = list}
    insertActualParameter a@CondExpr{} localVars =
      CondExpr {eiff = insertActualParameter (eiff a) localVars,
                ethenn = insertActualParameter (ethenn a) localVars,
                eelsee = insertActualParameter (eelsee a) localVars}
    insertActualParameter a@AssignExpr{} localVars =
      AssignExpr {assEleft = assEleft a,
                  assEright = insertActualParameter (assEright a) localVars}
    insertActualParameter a@ReturnExpr{returnE = Just aa} localVars = insertActualParameter aa localVars
    insertActualParameter a _ = a

    boolBinOpEvaluate :: [Expression] -> Expression -> BinOp -> Expression -> Bool
    boolBinOpEvaluate localVars expr1 binOp expr2 = case evaluate' (evaluate expr1 localVars) binOp (evaluate expr2 localVars) of
      BoolLiteral True -> True
      BoolLiteral False -> False

    boolUnOpEvaluate :: [Expression] -> Expression -> Bool
    boolUnOpEvaluate localVars expr = case evaluate expr localVars of
      BoolLiteral True -> False
      BoolLiteral False -> True

    -- evaluates an expression by turning it to its correspondent literal value.
    -- This function is used while fully knowing that the expression CAN be evaluated
    evaluate :: Expression -> [Expression] -> Expression
    evaluate a@(IntLiteral _) _ = a
    evaluate a@(StringLiteral _) _ = a
    evaluate a@(BoolLiteral _) _ = a
    evaluate BinOpExpr{expr1,binOp,expr2} localVars =
      let left = evaluate expr1 localVars
          right = evaluate expr2 localVars
      in evaluate' left binOp right
    evaluate UnOpExpr{expr} localVars = evaluate (negate expr) localVars
    evaluate a@VarExpr{} localVars =
      let filtered = filter (\case AssignExpr{assEleft, assEright} -> varName a == varName assEleft) localVars
          extractedLiteral = assEright $ head filtered --throw $ NoteExcp $ printf "\n___\n%s\n___\n" (show a)
      in evaluate extractedLiteral localVars
      --in if null filtered then
         --else
    evaluate FunCallExpr{} _ = undefined
    evaluate CondExpr{} _ = undefined
    evaluate AssignExpr{} _ = undefined
    evaluate ExcpExpr{} _ = undefined
    evaluate ReturnExpr{} _ = undefined

    -- helps dealing with binary operations.
    -- All passed expressions from `evaluate` to this function are literals.
    -- Ergo no more recursive evaluation is needed, because `evaluete` takes care of that
    evaluate' :: Expression -> BinOp -> Expression -> Expression
    evaluate' expr1 binOp expr2 = case binOp of
      Plus ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in IntLiteral $ n1+n2
      Mult ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in IntLiteral $ n1*n2
      Minus ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in IntLiteral $ n1-n2
      Div ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in undefined
      Mod ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in IntLiteral $ n1 `mod` n2
      Less ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in BoolLiteral $ n1<n2
      LessEq ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in BoolLiteral $ n1<=n2
      boolBinOps | isJust (ofSimilarTypes expr1 expr2) -> case (boolBinOps, fromJust $ ofSimilarTypes expr1 expr2) of
        (Greater,Int) ->
          let (IntLiteral n1) = expr1
              (IntLiteral n2) = expr2
          in BoolLiteral $ n1>n2
        (Greater,Boolean) ->
          let (BoolLiteral n1) = expr1
              (BoolLiteral n2) = expr2
          in BoolLiteral $ n1>n2
        (Greater,Char) ->
          let (CharLiteral n1) = expr1
              (CharLiteral n2) = expr2
          in BoolLiteral $ n1>n2
        (Greater,String) ->
          let (StringLiteral n1) = expr1
              (StringLiteral n2) = expr2
          in BoolLiteral $ n1>n2
        --------------------------------
        (GreaterEq,Int) ->
          let (IntLiteral n1) = expr1
              (IntLiteral n2) = expr2
          in BoolLiteral $ n1>=n2
        (GreaterEq,Boolean) ->
          let (BoolLiteral n1) = expr1
              (BoolLiteral n2) = expr2
          in BoolLiteral $ n1>=n2
        (GreaterEq,Char) ->
          let (CharLiteral n1) = expr1
              (CharLiteral n2) = expr2
          in BoolLiteral $ n1>=n2
        (GreaterEq,String) ->
          let (StringLiteral n1) = expr1
              (StringLiteral n2) = expr2
          in BoolLiteral $ n1>=n2
        --------------------------------
        (Eq,Int) ->
          let (IntLiteral n1) = expr1
              (IntLiteral n2) = expr2
          in BoolLiteral $ n1==n2
        (Eq,Boolean) ->
          let (BoolLiteral n1) = expr1
              (BoolLiteral n2) = expr2
          in BoolLiteral $ n1==n2
        (Eq,Char) ->
          let (CharLiteral n1) = expr1
              (CharLiteral n2) = expr2
          in BoolLiteral $ n1==n2
        (Eq,String) ->
          let (StringLiteral n1) = expr1
              (StringLiteral n2) = expr2
          in BoolLiteral $ n1==n2
        --------------------------------
        (Neq,Int) ->
          let (IntLiteral n1) = expr1
              (IntLiteral n2) = expr2
          in BoolLiteral $ n1/=n2
        (Neq,Char) ->
          let (CharLiteral n1) = expr1
              (CharLiteral n2) = expr2
          in BoolLiteral $ n1/=n2
        (Neq,Boolean) ->
          let (BoolLiteral n1) = expr1
              (BoolLiteral n2) = expr2
          in BoolLiteral $ n1/=n2
        (Neq,String) ->
          let (StringLiteral n1) = expr1
              (StringLiteral n2) = expr2
          in BoolLiteral $ n1/=n2
        --------------------------------
        (And,Boolean) ->
          let (BoolLiteral n1) = expr1
              (BoolLiteral n2) = expr2
          in BoolLiteral $ n1&&n2
        --------------------------------
        (Or,Boolean) ->
          let (BoolLiteral n1) = expr1
              (BoolLiteral n2) = expr2
          in BoolLiteral $ n1||n2

    -- checks whether the two passed expressions are homogeneous.
    -- If they are homogeneous, then it's Just <Types>
    -- If not, then it's Nothing
    ofSimilarTypes :: Expression -> Expression -> Maybe Types
    ofSimilarTypes (IntLiteral    _) (IntLiteral _)     = Just Int
    ofSimilarTypes (IntLiteral    _) _                  = Nothing
    ofSimilarTypes (BoolLiteral   _) (BoolLiteral _)   = Just Boolean
    ofSimilarTypes (BoolLiteral   _) _                 = Nothing
    ofSimilarTypes (CharLiteral   _) (CharLiteral _)   = Just Char
    ofSimilarTypes (CharLiteral   _) _                 = Nothing
    ofSimilarTypes (StringLiteral _) (StringLiteral _) = Just String
    ofSimilarTypes (StringLiteral _) _                 = Nothing

    isLiteral :: Expression -> Bool
    isLiteral (IntLiteral _) = True
    isLiteral (BoolLiteral _) = True
    isLiteral (CharLiteral _) = True
    isLiteral (StringLiteral _) = True
    isLiteral _ = False
-- copulate each parameter with its actual value
-- in case the function with the passed name was internally called
copulateVarNameToActualParameters :: [ExternalDeclaration] -> String -> [Expression] -> [Expression]
copulateVarNameToActualParameters extDeclList funName actParExprList =
  let funFound = filter (\case FunDef{funDecl=FunCallStmt{funCall=FunCallExpr{funName=VarExpr{varName}}}} -> varName == funName) extDeclList
  in if null funFound then throw $ NoteExcp $ printf "{{ToJML -> attachVarNameToActualParameters}}: searched function name wasn't found" funName
     else
       let FunDef{funDecl=FunCallStmt{funCall=FunCallExpr{funArgs}}} = head funFound
       in zipWith (\l r -> AssignExpr{assEleft=l,assEright=r}) funArgs actParExprList