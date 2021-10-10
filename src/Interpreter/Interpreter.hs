{-# Language LambdaCase, NamedFieldPuns #-}
module Interpreter.Interpreter(
  enforceActualParameterEvaluation,
  enforceLocalVariablesEvaluation
) where

import Parser.Types
import JML.RefineParsed
import Data.Maybe
import Control.Exception
import Text.Printf(printf)
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
  return 5;
}

and when foo(-5):
int foo(int i){
  return (-1)*5;
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
          in CompStmt{statements = enforceEvaluation theElses (localVars ++ getCompStmtLocalVariables' theElses)}
      -- delete else
      | fst (evaluate_if_else (condition a) localVars) &&
        snd (snd $ evaluate_if_else (condition a) localVars) =
          let CompStmt{statements = theIfs} = siff a
          in CompStmt{statements = enforceEvaluation theIfs (localVars ++ getCompStmtLocalVariables' theIfs)}
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
    f2 a@WhileStmt{} localVars
      | isJust (lookUpVar'' (condition a) localVars) &&
        cannotEvaluate (condition a) (fromJust $ lookUpVar'' (condition a) localVars) =
          let stmts = statements $ whileBody a
          in WhileStmt{condition = insertActualParameter (condition a) localVars,
                       whileBody = CompStmt{statements = enforceEvaluation stmts (localVars ++ getCompStmtLocalVariables' stmts)}}
      --while's conditional expression is evaluation and gives true, therefore the while is to be deleted.
      | fst (evaluate_if_else (condition a) localVars) &&
        fst (snd $ evaluate_if_else (condition a) localVars) =
          WhileStmt{condition = BoolLiteral False,
                    whileBody = CompStmt{statements=[]}}
      --while's conditional expression is evaluation and gives false, therefore the while is to be deleted.
      | fst (evaluate_if_else (condition a) localVars) &&
        snd (snd $ evaluate_if_else (condition a) localVars) =
          let stmts = statements $ whileBody a
          in WhileStmt{condition = BoolLiteral True,
                       whileBody = CompStmt{statements = enforceEvaluation stmts (localVars ++ getCompStmtLocalVariables' stmts)}}
      | not (fst $ evaluate_if_else (condition a) localVars) =
          let stmts = statements $ whileBody a
          in WhileStmt{condition = condition a,
                       whileBody = CompStmt{statements = enforceEvaluation stmts (localVars ++ getCompStmtLocalVariables' stmts)}}
    f2 a@TryCatchStmt{} localVars =
      let interpretedTry = enforceEvaluation (statements $ tryBody a) (getCompStmtLocalVariables' (statements $ tryBody a))
          thrownException = exceptionIsThrown interpretedTry []
      in if not $ null thrownException
           then let catchedExcp = Exception $ typee $ catchExcp a
                in if catchedExcp `elem` thrownException
                     then CompStmt{statements = statements (catchBody a) ++ statements (finallyBody a)}
                   else let CompStmt{statements = tryOrgBody} = tryBody a
                            CompStmt{statements = catchOrgBody} = catchBody a
                            CompStmt{statements = finallyOrgBody} = finallyBody a
                        in TryCatchStmt{tryBody = CompStmt{statements = enforceEvaluation tryOrgBody (localVars ++ getCompStmtLocalVariables' tryOrgBody)},
                                        catchExcp = catchExcp a,
                                        catchBody = CompStmt{statements = enforceEvaluation catchOrgBody (localVars ++ getCompStmtLocalVariables' catchOrgBody)},
                                        finallyBody = CompStmt{statements = enforceEvaluation finallyOrgBody (localVars ++ getCompStmtLocalVariables' finallyOrgBody)}}
         else let CompStmt{statements = tryOrgBody} = tryBody a
                  CompStmt{statements = catchOrgBody} = catchBody a
                  CompStmt{statements = finallyOrgBody} = finallyBody a
              in TryCatchStmt{tryBody = CompStmt{statements = enforceEvaluation tryOrgBody (localVars ++ getCompStmtLocalVariables' tryOrgBody)},
                              catchExcp = catchExcp a,
                              catchBody = CompStmt{statements = enforceEvaluation catchOrgBody (localVars ++ getCompStmtLocalVariables' catchOrgBody)},
                              finallyBody = CompStmt{statements = enforceEvaluation finallyOrgBody (localVars ++ getCompStmtLocalVariables' finallyOrgBody)}}
    f2 ReturnStmt{returnS = Just a} localVars = ReturnStmt{returnS = Just $ insertActualParameter a localVars} --throw $ NoteExcp $ printf "\n___\n%s\n___\n" (show $ insertActualParameter a localVars)
    f2 stmt localVars = stmt

    exceptionIsThrown :: [Statement] -> [Parser.Types.Exception] -> [Parser.Types.Exception]
    exceptionIsThrown [] res = res
    exceptionIsThrown (a@CompStmt{}:rest) res = exceptionIsThrown (statements a ++ rest) res
    exceptionIsThrown (VarStmt{}:rest) res = exceptionIsThrown rest res
    exceptionIsThrown (AssignStmt _ assign:rest) res = case assign of
      a@ExcpExpr{} -> exceptionIsThrown rest (res ++ [excpName a])
      _            -> exceptionIsThrown rest res
    exceptionIsThrown (CondStmt{}:rest) res = exceptionIsThrown rest res
    exceptionIsThrown (a@ForStmt{}:rest) res = exceptionIsThrown (statements (forBody a) ++ rest) res
    exceptionIsThrown (a@WhileStmt{}:rest) res = exceptionIsThrown (statements (whileBody a) ++ rest) res
    exceptionIsThrown (TryCatchStmt{}:rest) res = throw $ NoteExcp "interpretation of nested try-catch-statement in not implemented yet"
    exceptionIsThrown (ReturnStmt{returnS = Just a@ExcpExpr{}}:rest) res = exceptionIsThrown rest (res ++ [excpName a])
    exceptionIsThrown (ReturnStmt{}:rest) res = exceptionIsThrown rest res
    
    
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
         else a2
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
    boolUnOpEvaluate localVars expr = not $ fromBoolLiteral (evaluate expr localVars)

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
    evaluate UnOpExpr{expr} localVars = evaluate (negateBoolLiteral expr) localVars
    evaluate a@VarExpr{} localVars =
      let filtered = filter (\case AssignExpr{assEleft, assEright} -> varName a == varName assEleft) localVars
          extractedLiteral = assEright $ head filtered
      in evaluate extractedLiteral localVars
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
