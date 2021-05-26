{-# Language LambdaCase,NamedFieldPuns #-}
module JML.RefineParsed where
import Parser.Types
import Prelude hiding(negate)
import Control.Exception(throw)
import Data.Maybe(isNothing, fromJust, isJust)
import Parser.ParseExternalDeclarations
import Data.List.Split(splitOn)
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

-- copulate each parameter with its actual value
-- in case the function with the passed name was internally called
attachVarNameToActualParameters :: [ExternalDeclaration] -> String -> [Expression] -> [Expression]
attachVarNameToActualParameters extDeclList funName actParExprList =
  let funFound = filter (\case FunDef{funDecl=FunCallStmt{funCall=FunCallExpr{funName=VarExpr{varName}}}} -> varName == funName) extDeclList
  in if null funFound then throw $ NoteExcp $ printf "{{ToJML -> attachVarNameToActualParameters}}: searched function name wasn't found" funName
     else
       let FunDef{funDecl=FunCallStmt{funCall=FunCallExpr{funArgs}}} = head funFound
       in zipWith (\l r -> AssignExpr{assEleft=l,assEright=r}) funArgs actParExprList

-- looks up the function whose name is passed to this function.
-- After funding the function in the list of external declarations, this function will be altered in a way
-- that expressions which are not meant to be processed will be cut short.
-- p.s. it's implemented minimally, in the sense that only conditional statements will be alterd.
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
enforceActualParameters :: [ExternalDeclaration] -> String -> [Expression] -> [ExternalDeclaration]
enforceActualParameters extDeclList funName actParExprList = map f1 extDeclList
  where
    -- is meant as a high-order function,
    -- and helps looking up the function whose name was passed to `enforceActualParameters`,
    -- and solely altering it
    f1 :: ExternalDeclaration -> ExternalDeclaration
    f1 a@FunDef{funDecl=FunCallStmt{funCall=b@FunCallExpr{funName=VarExpr{varName}}},funBody=CompStmt{statements}}
      | varName == funName =
          let funAltered = alterFun statements
          in FunDef{funModifier = funModifier a,
                    isPureFlag  = isPureFlag a,
                    funDecl = FunCallStmt{funCall=b},
                    throws = throws a,
                    funBody = CompStmt{statements=funAltered}}
    f1 extDecl = extDecl

    -- takes the actual parameters into consideration to alter the function's statements
    -- Each statement belongs to the function, and only if-else statements (CondStmt) are being processed
    alterFun :: [Statement] -> [Statement]
    alterFun = map f2

    -- is meant as a high-order function,
    -- and helps looking up the addressed if-else statements and altering its body
    -- Due to the fact, the other nested statements could, be found, then this will be also examined
    f2 :: Statement -> Statement
    f2 a@CondStmt{}
      -- alter if
      | fst (evaluate_if_else $ condition a) &&
        fst (snd $ evaluate_if_else $ condition a) = CondStmt{condition = condition a,
                                                              siff = CompStmt{statements=[]},
                                                              selsee = selsee a}
      -- alter else
      | fst (evaluate_if_else $ condition a) &&
        snd (snd $ evaluate_if_else $ condition a) = CondStmt{condition = condition a,
                                                              siff = siff a,
                                                              selsee = CompStmt{statements=[]}}
      -- alter body of if and else in case the seeked CondStmt is nested
      | not (fst $ evaluate_if_else $ condition a) =
          let CompStmt{statements = theIfs}   = siff a
              CompStmt{statements = theElses} = selsee a
          in CondStmt{condition = condition a,
                      siff      = CompStmt{statements = alterFun theIfs},
                      selsee    = CompStmt{statements = alterFun theElses}}
    -- ForStmt {acc :: Statement, cond :: Expression, step :: Statement, forBody :: Statement}
    f2 a@ForStmt{} =
      let CompStmt{statements = orgBody} = forBody a
      in ForStmt{acc = acc a,
                 cond = cond a,
                 step = step a,
                 forBody = CompStmt{statements = alterFun orgBody}}
    -- WhileStmt {condition :: Expression, whileBody :: Statement}
    f2 a@WhileStmt{} =
      let CompStmt{statements = orgBody} = whileBody a
      in WhileStmt{condition = condition a,
                   whileBody = CompStmt{statements = alterFun orgBody}}
    f2 a@TryCatchStmt{} =
      let CompStmt{statements = tryOrgBody} = tryBody a
          CompStmt{statements = catchOrgBody} = catchBody a
          CompStmt{statements = finallyOrgBody} = finallyBody a
      in TryCatchStmt{tryBody = CompStmt{statements = alterFun tryOrgBody},
                      catchExcp = catchExcp a,
                      catchBody = CompStmt{statements = alterFun catchOrgBody},
                      finallyBody = CompStmt{statements = alterFun finallyOrgBody}}
    f2 stmt = stmt

    -- it examines the if-else statement, to know whether it's the one whose body should be altered.
    -- If it is the one, then (fst evaluate_if_else) == True. Otherwise its return type is (False,undefined)
    -- Also: If it is the one, and the if statement needs to altered,   then its return type is (True,(True,False))
    --       If it is the one, and the else statement needs to altered, then its return type is (True,(False,True))
    -- evaluate_if_else accepts as an Expression only BinOpExpr, UnOpExpr, BoolLiteral
    evaluate_if_else :: Expression -> (Bool,(Bool,Bool))
    evaluate_if_else BinOpExpr{expr1, binOp, expr2} =
      let searching = lookUpVar expr1 expr2
      in maybe (False,undefined) (\case
        1 -> if boolBinOpEvaluate (insertActualParameter expr1) binOp expr2
               then (True,(False,True))
             else (True,(True,False))
        2 -> if boolBinOpEvaluate expr1 binOp (insertActualParameter expr2)
               then (True,(False,True))
             else (True,(True,False))
        3 -> if boolBinOpEvaluate (insertActualParameter expr1) binOp (insertActualParameter expr2)
               then (True,(False,True))
             else (True,(True,False))) searching
    evaluate_if_else UnOpExpr{unOp, expr} = undefined
    evaluate_if_else (BoolLiteral True)  = (True,(False,True))
    evaluate_if_else (BoolLiteral False) = (True,(True,False))
    evaluate_if_else a = throw $ NoteExcp $ printf "\n___\n{{RefineParsed.hs -> enforceActualParameters -> evaluate_if_else}}: the passed Expression isn't right:\n%s\n___\n" (show a)

    -- looks up the arguments in
    -- If the two passed expressions are uneffected by the actual parameters, then its return is Nothing
    -- If the first passed argument has a parameter, then its return is Just 1
    -- If the second passed argument has a parameter, then its return is Just 2
    -- If both Expressions have parameters, then its return is Just 3
    lookUpVar :: Expression -> Expression -> Maybe Int
    lookUpVar expr1 expr2 = case map hasParam [expr1,expr2] of
      [True,True] -> Just 3
      [True,False] -> Just 1
      [False,True] -> Just 2
      [False,False] -> Nothing

    -- checks whether an Expression has an actual parameter
    hasParam :: Expression -> Bool
    hasParam (IntLiteral _)  = False
    hasParam (BoolLiteral _) = False
    hasParam (CharLiteral _) = False
    hasParam (StringLiteral _) = False
    hasParam Null = False
    hasParam VarExpr{varName} = any (\case AssignExpr {assEleft=VarExpr{varName = argVarName}} -> varName == argVarName) actParExprList
    hasParam ArrayExpr{arrName} = hasParam arrName
    hasParam BinOpExpr{expr1,expr2} = any hasParam [expr1,expr2]
    hasParam UnOpExpr{expr} = hasParam expr
    hasParam FunCallExpr{} = False
    hasParam CondExpr{eiff,ethenn,eelsee} = any hasParam [eiff,ethenn,eelsee]
    hasParam AssignExpr{assEleft,assEright} = any hasParam [assEleft,assEright]
    hasParam ExcpExpr{} = throw $ NoteExcp "\n___\n{{RefineParsed.hs -> enforceActualParameter -> hasParam -> ExcpExpr}}: Unimplemented\n___\n"
    hasParam ReturnExpr{returnE = Nothing} = False
    hasParam ReturnExpr{returnE = Just a} = hasParam a

    -- replaces VarExpr with the correspondent literal taken from the list of actual parameters
    insertActualParameter :: Expression -> Expression
    insertActualParameter a@VarExpr{} =
      let filtered = filter (\case AssignExpr{assEleft=b@VarExpr{}} -> varName a == varName b) actParExprList
      in if null filtered then a
         else assEright (head filtered)

    boolBinOpEvaluate :: Expression -> BinOp -> Expression -> Bool
    boolBinOpEvaluate expr1 binOp expr2 = case evaluate' (evaluate expr1) binOp (evaluate expr2) of
      BoolLiteral True -> True
      BoolLiteral False -> False

    -- evaluates an expression by turning it to its correspondent literal value
    evaluate :: Expression -> Expression
    evaluate a@(IntLiteral _) = a
    evaluate a@(StringLiteral _) = a
    evaluate BinOpExpr{expr1,binOp,expr2} =
      let left = evaluate expr1
          right = evaluate expr2
      in evaluate' left binOp right
    evaluate UnOpExpr{expr} = negate expr
    evaluate VarExpr{} = undefined
    evaluate FunCallExpr{} = undefined
    evaluate CondExpr{} = undefined
    evaluate AssignExpr{} = undefined
    evaluate ExcpExpr{} = undefined
    evaluate ReturnExpr{} = undefined

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
      Greater ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in BoolLiteral $ n1>n2
      GreaterEq ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in BoolLiteral $ n1>=n2
      Eq ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in BoolLiteral $ n1==n2
      Neq ->
        let (IntLiteral n1) = expr1
            (IntLiteral n2) = expr2
        in BoolLiteral $ n1/=n2
      And ->
        let (BoolLiteral n1) = expr1
            (BoolLiteral n2) = expr2
        in BoolLiteral $ n1&&n2
      Or ->
        let (BoolLiteral n1) = expr1
            (BoolLiteral n2) = expr2
        in BoolLiteral $ n1||n2