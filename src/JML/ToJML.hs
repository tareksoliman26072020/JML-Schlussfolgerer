{-# Language LambdaCase, NamedFieldPuns #-}
module JML.ToJML where

import JML.JMLTypes as JMLTypes
import Parser.Types
import JML.RefineParsed
import qualified Prelude as Maybe (Maybe(Nothing))
import Prelude hiding(negate)
import Data.Maybe(fromMaybe, isNothing, isJust,fromJust,catMaybes)
import Control.Exception(throw)
import Text.Printf
import Data.List(isPrefixOf,foldl')
import Interpreter.Interpreter

isPure :: [ExternalDeclaration] -> ExternalDeclaration -> Bool
isPure extDeclList lV@(FunDef _ _ (FunCallStmt (FunCallExpr (VarExpr varType _ _) _)) _ funBody)
  | isNothing varType ||
    (isJust varType && fromJust varType /= BuiltInType Void) = process2 (getFunLocalVariables lV) funBody
    --if the function is of void, then it's not pure:
  | varType == Just (BuiltInType Void)   = False
  | otherwise = throw $ NoteExcp "{{isPure}}: one or more cases were not discussed"
  where
    -- At this point the function isn't of void.
    -- first argument is the local variables
    -- second argument is funBody
    -- figure out whether this is pure or not by going through the function's body
    process2 :: [String] -> Statement -> Bool
    process2 localVariables (CompStmt list) = all (process3 localVariables) list
    process2 _ _ = throw $ NoteExcp "{{isPure -> process2}}: (-/> CompStmt)"

    -- At this point the function isn't of void.
    -- first argument is the local variables
    -- second argument is funBody
    -- third argument is every statement in the function. it's never CompStmt
    -- figure out whether this is pure or not by going through the function's
    process3 :: [String] -> Statement -> Bool
    process3 localVariables (VarStmt a@VarExpr{}) = process4 localVariables a
    process3 localVariables VarStmt{} = throw $ NoteExcp "{{isPure -> process3}}: -> VarStmt (-/>VarExpr)"
    process3 localVariables (AssignStmt _ a@(AssignExpr left right)) = process4 localVariables a
    process3 _ AssignStmt{} = throw $ NoteExcp "{{isPure -> process3}}: -> AssignStmt -/> AssignExpr"
    process3 localVariables (CondStmt condition siff selsee) =
      process4 localVariables condition &&
      all (\stmt -> process2 (localVariables ++ getCompStmtLocalVariables stmt) stmt) [siff,selsee]
    process3 localVariables (ForStmt AssignStmt{assign=AssignExpr{assEleft=VarExpr{varName}}} cond _ stmtBody) =
      let newLocalVariables = localVariables ++ [varName] ++ getCompStmtLocalVariables stmtBody
      in process4 newLocalVariables cond && process2 newLocalVariables stmtBody
    process3 localVariables (WhileStmt _ whileBody) = process2 (localVariables ++ getCompStmtLocalVariables whileBody) whileBody
    process3 localVariables (FunCallStmt (FunCallExpr (VarExpr _ _ varName) _)) = case findParsedFunction varName extDeclList of--isPure extDeclList varName
      Maybe.Nothing   -> throw $ NoteExcp $ printf "{{isPure}}: searched function:%s wasn't found" varName
      Just newExtDecl -> isPure extDeclList newExtDecl
    process3 localVariables (FunCallStmt FunCallExpr{}) = throw $ NoteExcp "{{isPure -> process3}}: FunCallStmt (FunCallExpr (-/>VarExpr) _)"
    process3 localVariables a@FunCallStmt{} = throw $ NoteExcp $ printf "{{isPure -> process3}}: FunCallStmt (-/>FunCallExpr): %s" (show a)
    process3 localVariables TryCatchStmt{} = False
      --all (process2 localVariables) [tryBody,catchBody,finallyBody]
    process3 localVariables (ReturnStmt Maybe.Nothing) = True
    process3 localVariables (ReturnStmt (Just expression)) = process4 localVariables expression

    --evaluate whether the expression is impure
    --the to-be-evaluted expression is the right side of some assignment, or some random expression
    -- first argument: local variables
    -- second argument: some expression
    process4 :: [String] -> Expression -> Bool
    process4 localVariables (VarExpr Maybe.Nothing _ varName) = varName `elem` localVariables
    process4 localVariables (ArrayExpr VarExpr{} Maybe.Nothing) = True
    process4 localVariables (ArrayExpr VarExpr{} (Just idx)) = process4 localVariables idx
    process4 localVariables (ArrayExpr _ index) = throw $ NoteExcp "{{isPure -> process4}}: ArrayExpr (-/> VarExpr) _"
    process4 localVariables (BinOpExpr expr1 _ expr2) = all (process4 localVariables) [expr1,expr2]
    process4 localVariables (UnOpExpr _ expr) = process4 localVariables expr
    process4 localVariables (FunCallExpr (VarExpr _ _ varName) _) = case findParsedFunction varName extDeclList of
      Maybe.Nothing   -> throw $ NoteExcp $ printf "{{isPure}}: searched function:%s wasn't found" varName
      Just newExtDecl -> isPure extDeclList newExtDecl
    process4 localVariables FunCallExpr{} = throw $ NoteExcp "{{isPure -> process4}}: FunCallExpr (-/> VarExpr) _"
    process4 localVariables (CondExpr eiff ethenn elsee) = all (process4 localVariables) [eiff,ethenn,elsee]
    process4 localVariables (AssignExpr (VarExpr Maybe.Nothing _ varName1) assEright)
      | varName1 `notElem` localVariables = False
      | otherwise = process4 localVariables assEright
    process4 localVariables (AssignExpr VarExpr{} assEright) = process4 localVariables assEright
    process4 localVariables AssignExpr{} = throw $ NoteExcp "{{isPure -> process4}}: AssignExpr (-/> VarExpr) _"
    process4 localVariables (ExcpExpr excpName excpmsg) = False
    process4 localVariables (ReturnExpr Maybe.Nothing) = True
    process4 localVariables (ReturnExpr (Just expr)) = process4 localVariables expr
    process4 _ _ = True

jmlify :: [ExternalDeclaration] -> [([JMLSyntax], ExternalDeclaration)]
jmlify extDeclList =
  let mutatedExtDeclList = map (highlightGlobalVariables . enforceLocalVariablesEvaluation) extDeclList
  in map (f mutatedExtDeclList) extDeclList
  where
    f :: [ExternalDeclaration] -> ExternalDeclaration -> ([JMLSyntax], ExternalDeclaration)
    f mutatedExtDeclList extDecl =
      let fun_name = varName $ funName $ funCall $ funDecl extDecl
          jmlify_ = toJMLs (isPure extDeclList extDecl) (getAllGlobalVariable $ enforceLocalVariablesEvaluation extDecl) $ getRequireEnsureBehavior (False,Maybe.Nothing,False) mutatedExtDeclList fun_name
      in (jmlify_,FunDef{funModifier=funModifier extDecl,
                         isPureFlag=isPure extDeclList extDecl,
                         funDecl=funDecl extDecl,
                         throws=throws extDecl,
                         funBody=funBody extDecl})

-- called is meant to know whether the function was internally called by another function, or not
-- enforced is True: when the function is internally called by another function, and the function was altered to mind its actual parameters
-- enforce is False: when the function is not internally called by another function
-- enforce is False: when the function is internally called by another function, and its actual parameters weren't enforced yet on the passed list of external declarations
getRequireEnsureBehavior :: (Bool,Maybe [Expression],Bool) -> [ExternalDeclaration] -> String -> [(Maybe Exception,JMLExpr,Maybe JMLExpr)]
getRequireEnsureBehavior (called,maybeActualParameters,enforced) extDeclList funName =
    let getFunExtDecl :: Maybe ExternalDeclaration
        getFunExtDecl = findParsedFunction (getOriginalFunName funName) extDeclList
    in case getFunExtDecl of
      Maybe.Nothing -> throw $ NoteExcp $ printf "{{getRequireEnsureBehavior}}: searched function:%s wasn't found" funName
      Just lv@(FunDef _ _ (FunCallStmt (FunCallExpr VarExpr{} funArgs)) _ funBody) ->
        if not called then refineRes $ process0 [] funArgs (fromVarExprToString funArgs) (getFunLocalVariables lv) (BoolLiteral True) funBody else
        if enforced then refineRes $ process0 [] funArgs (attachFunName1 funName $ fromVarExprToString funArgs) (attachFunName1 funName $ getFunLocalVariables lv) (BoolLiteral True) (attachFunName2 funName funBody)
        else getRequireEnsureBehavior (called,maybeActualParameters,True) (enforceActualParameterEvaluation extDeclList funName (fromJust maybeActualParameters)) funName
  where
    --process0 is the body of the function.
    --therefore it's of CompStmt
    process0 :: [Statement] -> [Expression] -> [String] -> [String] -> Expression -> Statement -> [(Maybe Exception,JMLExpr,Maybe JMLExpr)]
    process0 stmts funArgs funArgsLV lv condExpr (CompStmt list) = concatMap (process1 (stmts ++ list) funArgs funArgsLV lv condExpr) list
    process0 _ _ _ _ _ _ = throw $ NoteExcp "{{getRequireEnsureBehavior -> process1}}: (-/> CompStmt)"

    process1 :: [Statement] -> [Expression] -> [String] -> [String] -> Expression -> Statement -> [(Maybe Exception,JMLExpr,Maybe JMLExpr)]
    process1 stmts funArgs funArgsLV lv condExpr (a@CompStmt{}) = process0 stmts funArgs funArgsLV lv condExpr a--throw $ NoteExcp "meow"
    process1 stmts _ _ lv condExpr (VarStmt expr) = process2 stmts lv condExpr expr
    process1 stmts _ _ lv condExpr (AssignStmt _ assign) =
      filter (\(a,_,_) -> isJust a) $ process2 stmts lv condExpr assign
    process1 stmts funArgs funArgsLV lv condExpr (CondStmt condition ifComp elseComp) = --throw $ NoteExcp $ printf "162:\n%s" (show stmts)
      let requires1 = refineRes $ process0 stmts funArgs funArgsLV (lv ++ getCompStmtLocalVariables ifComp) (appendBoolExprRight condExpr condition) ifComp
          requires2 = refineRes $ process0 stmts funArgs funArgsLV (lv ++ getCompStmtLocalVariables elseComp) (appendBoolExprRight condExpr (negateBoolLiteral condition)) elseComp
      in requires1 ++ requires2
    process1 stmts funArgs funArgsLV lv condExpr (ForStmt AssignStmt{assign=AssignExpr{assEleft=VarExpr{varName}}} cond _ forBody) =
      let x = refineRes $ process0 stmts funArgs funArgsLV (lv ++ [varName] ++ getCompStmtLocalVariables forBody) (appendBoolExprRight condExpr cond) forBody
      in x
    process1 stmts funArgs funArgsLV lv condExpr (WhileStmt condition whileBody) =
      let x = refineRes $ process0 stmts funArgs funArgsLV (lv ++ getCompStmtLocalVariables whileBody) (appendBoolExprRight condExpr condition) whileBody
      in x
    process1 stmts _ _ lv condExpr (FunCallStmt funCall) = process2 stmts lv condExpr funCall
    process1 stmts funArgs funArgsLV lv condExpr (TryCatchStmt tryBody _ catchBody finallyBody) =
      concatMap (\comp -> refineRes $ process0 stmts funArgs funArgsLV (lv ++ getCompStmtLocalVariables comp) condExpr comp) [tryBody,catchBody,finallyBody]
    process1 stmts _ _ lv condExpr (ReturnStmt Maybe.Nothing) = [(Maybe.Nothing,JMLExpr condExpr,Maybe.Nothing)]
    process1 stmts _ funArgsLV lv condExpr a@(ReturnStmt (Just expr)) =
      let requires = process2 stmts lv condExpr expr
          ensures  = getEnsures extDeclList stmts funArgsLV lv expr
      in insertEnsures requires ensures
      {-case expr of
        FunCallExpr {funName = VarExpr {varType = Nothing, varObj = [], varName = "boo17"}, funArgs = []} -> throw $ NoteExcp $ printf "190:\n%s" (show condExpr)
        _ ->insertEnsures requires ensures-}

    --processing Expressions resulted from process1
    process2 :: [Statement] -> [String] -> Expression -> Expression -> [(Maybe Exception,JMLExpr,Maybe JMLExpr)]
    process2 stmts lv condExpr a@(VarExpr (Just _) _ varName) = [(Maybe.Nothing,undefined,Maybe.Nothing)] --TODO: is this right?
    process2 _ lv condExpr a@(VarExpr Maybe.Nothing _ varName) =
      if varName `notElem` lv then throw $ NoteExcp $ printf "{{getRequireEnsureBehavior -> process2}}: VarExpr _ varName: %s is not a local variable" varName
      else [(Maybe.Nothing,JMLExpr condExpr,Maybe.Nothing)]
    process2 _ lv condExpr a@(ArrayExpr (VarExpr _ _ varName) Maybe.Nothing)
      | varName `elem` lv = [(Maybe.Nothing,JMLExpr condExpr,Just $ JMLExpr a)]
    process2 stmts lv condExpr (ArrayExpr _ (Just expr)) = process2 stmts lv condExpr expr
    process2 stmts lv condExpr (BinOpExpr expr1 _ expr2) = process2 stmts lv condExpr expr1 ++ process2 stmts lv condExpr expr2
    process2 stmts lv condExpr (UnOpExpr _ expr) = process2 stmts lv (negateBoolLiteral condExpr) expr
    process2 _ lv condExpr FunCallExpr{funName=VarExpr{varName},funArgs} =
      let rec = getRequireEnsureBehavior (True,Just $ copulateVarNameToActualParameters extDeclList varName funArgs,False) extDeclList varName
      in refineRes $ appendOriginalCondExpr rec varName condExpr
    process2 stmts lv condExpr (CondExpr eiff ethenn elsee) =
      let x1 = process2 stmts lv (appendBoolExprRight condExpr eiff) ethenn
          x2 = process2 stmts lv (appendBoolExprRight condExpr (negateBoolLiteral eiff)) elsee
      in x1 ++ x2
    process2 stmts lv condExpr (AssignExpr exprL exprR) =
      let x1 = process2 stmts lv condExpr exprR
      in x1--assignExists lv exprL `seq` x1
      {-case exprL of --deleteEnsures x1
      BoolLiteral _   -> deleteEnsures x1
      UnOpExpr _ expr ->-}
    process2 _ lv condExpr (ExcpExpr excpName _) = [(Just excpName,JMLExpr condExpr,Maybe.Nothing)]
    process2 _ lv condExpr (ReturnExpr Maybe.Nothing) = throw $ NoteExcp "this should be unreachable"--[(Nothing,JMLExpr condExpr,Nothing)]
    process2 _ lv condExpr (ReturnExpr (Just _)) = throw $ NoteExcp "this should be unreachable"
    process2 _ _ condExpr a = [(Maybe.Nothing,JMLExpr condExpr,Maybe.Nothing)]

    assignExists :: [String] -> Expression -> ()
    assignExists lv (VarExpr Maybe.Nothing _ varName)
      | varName `notElem` lv = ()--throw $ NoteExcp $ printf "{{getRequireEnsureBehavior -> process2 -> boolExprExists}}: VarExpr _ _ varName: %s does not exist" varName
      | otherwise = ()
    assignExists lv (VarExpr _ _ varName) = ()
    --assignExists lv a = throw $ NoteExcp $ printf "{{beobeobeo}}:\n%s" (show a)

    appendOriginalCondExpr :: [(Maybe Exception,JMLExpr,Maybe JMLExpr)] -> String -> Expression -> [(Maybe Exception,JMLExpr,Maybe JMLExpr)]
    appendOriginalCondExpr old funName condExpr =
      let hasFunName :: Expression -> Bool
          hasFunName (BoolLiteral _) = False
          hasFunName (BinOpExpr expr1 _ expr2) = hasFunName expr1 || hasFunName expr2
          hasFunName (UnOpExpr _ expr) = hasFunName expr
          hasFunName (VarExpr _ _ varName) = --varName == funName
            let splitIt = takeWhile (/='$') varName
            in splitIt == funName
          hasFunName expr = False--throw $ NoteExcp $ printf "{{getRequireEnsureBehavior -> process2 -> appendOriginalCondExpr}}:\n%s" (show expr)
          f :: (Maybe Exception,JMLExpr,Maybe JMLExpr) -> (Maybe Exception,JMLExpr,Maybe JMLExpr)
          f tuple@(one,JMLExpr two,three) | hasFunName two = (one,JMLExpr $ BinOpExpr condExpr And two,three)
          f tuple                                          = tuple
      in map f old

    refineRes :: [(Maybe Exception,JMLExpr,Maybe JMLExpr)] -> [(Maybe Exception,JMLExpr,Maybe JMLExpr)]
    refineRes list = map (\(l1,JMLExpr l2,l3) -> (l1,JMLExpr (refine l2),l3))list where
         refine :: Expression -> Expression
         refine (BinOpExpr (BoolLiteral True) And expr2) = expr2
         refine (BinOpExpr expr1 And expr2) = BinOpExpr (refine expr1) And expr2
         refine expr = expr

    insertEnsures :: [(Maybe Exception,JMLExpr,Maybe JMLExpr)] -> [Maybe JMLExpr] -> [(Maybe Exception,JMLExpr,Maybe JMLExpr)]
    insertEnsures requires ensures = foo requires ensures [] where
      foo :: [(Maybe Exception,JMLExpr,Maybe JMLExpr)] -> [Maybe JMLExpr] -> [(Maybe Exception,JMLExpr,Maybe JMLExpr)] -> [(Maybe Exception,JMLExpr,Maybe JMLExpr)]
      foo [] _ res = res
      foo _ [] res = res
      foo ((Maybe.Nothing,b,_):rest) (ens:restt) res = foo rest restt (res ++ [(Maybe.Nothing,b,ens)])
      foo (a:rest) restt res = foo rest restt (res ++ [a])

    attachFunName1 :: String -> [String] -> [String]
    attachFunName1 funName lv = map (\str->funName++"$"++str) lv

    attachFunName2 :: String -> Statement -> Statement
    attachFunName2 funName (CompStmt list) = CompStmt $ map (attachFunName2 funName) list
    attachFunName2 funName (VarStmt expr) = VarStmt (attachFunName2' funName expr)
    attachFunName2 funName (AssignStmt a expr) = AssignStmt a (attachFunName2' funName expr)
    attachFunName2 funName (CondStmt condition siff selsee) = CondStmt (attachFunName2' funName condition) (attachFunName2 funName siff) (attachFunName2 funName selsee)
    attachFunName2 funName (ForStmt acc cond step forBody) = ForStmt (attachFunName2 funName acc) (attachFunName2' funName cond) (attachFunName2 funName step) (attachFunName2 funName forBody)
    attachFunName2 funName (WhileStmt condition whileBody) = WhileStmt (attachFunName2' funName condition) (attachFunName2 funName whileBody)
    attachFunName2 funName (FunCallStmt funCallExpr) = FunCallStmt (attachFunName2' funName funCallExpr)
    attachFunName2 funName (TryCatchStmt tryBody catchExcp catchBody finallyBody) =
      TryCatchStmt (attachFunName2 funName tryBody) catchExcp (attachFunName2 funName catchBody) (attachFunName2 funName finallyBody)
    attachFunName2 funName (ReturnStmt Maybe.Nothing) = ReturnStmt Maybe.Nothing
    attachFunName2 funName (ReturnStmt (Just expr)) = ReturnStmt (Just (attachFunName2' funName expr))

    attachFunName2' :: String -> Expression -> Expression
    attachFunName2' funName (VarExpr a b varName) = VarExpr a b (funName++"$"++varName)
    attachFunName2' funName (ArrayExpr arrName Maybe.Nothing) = ArrayExpr (attachFunName2' funName arrName) Maybe.Nothing
    attachFunName2' funName (ArrayExpr arrName (Just index)) = ArrayExpr (attachFunName2' funName arrName)(Just $ attachFunName2' funName index)
    attachFunName2' funName (BinOpExpr expr1 binOp expr2) = BinOpExpr (attachFunName2' funName expr1) binOp (attachFunName2' funName expr2)
    attachFunName2' funName (UnOpExpr unOp expr) = UnOpExpr unOp (attachFunName2' funName expr)
    attachFunName2' funName (FunCallExpr fun'Name funArgs) = FunCallExpr (attachFunName2' funName fun'Name) (map (attachFunName2' funName) funArgs)
    attachFunName2' funName (CondExpr eiff ethenn elsee) = CondExpr (attachFunName2' funName eiff) (attachFunName2' funName ethenn) (attachFunName2' funName elsee)
    attachFunName2' funName (AssignExpr expr1 expr2) = AssignExpr (attachFunName2' funName expr1) (attachFunName2' funName expr2)
    attachFunName2' funName (ExcpExpr excpName excpmsg) = ExcpExpr excpName excpmsg
    attachFunName2' funName (ReturnExpr Maybe.Nothing) = ReturnExpr Maybe.Nothing
    attachFunName2' funName (ReturnExpr (Just expr)) = ReturnExpr (Just $ attachFunName2' funName expr)
    attachFunName2' _ expr = expr

    getOriginalFunName :: String -> String
    getOriginalFunName str
      | '$' `notElem` str = str
      | otherwise = takeWhile (/='$') str

--Übergeben wird ist die Liste der Statements, in der sich das übergebene Expression befindet
getEnsures :: [ExternalDeclaration] -> [Statement] -> [String] -> [String] -> Expression -> [Maybe JMLExpr]
getEnsures extDeclList stmts funArgs lv a = case a of
  IntLiteral _                                  -> [Just $ JMLExpr a]
  BoolLiteral _                                 -> [Just $ JMLExpr a]
  CharLiteral _                                 -> [Just $ JMLExpr a]
  StringLiteral _                               -> [Just $ JMLExpr a]
  Null                                          -> [Just $ JMLExpr Null]
  FunCallExpr{funName=VarExpr{varName},funArgs} -> process1 (getRequireEnsureBehavior (True,Just $ copulateVarNameToActualParameters extDeclList varName funArgs,False) extDeclList varName) extDeclList varName funArgs
{-    | isPure extDeclList funName -> process1 (getRequireEnsureBehavior True extDeclList funName) extDeclList funName funArgsList
    | otherwise              -> [Nothing]-}
  VarExpr (Just _) _ _ -> throw $ NoteExcp "{{getEnsures}}: VarExpr (Just _) _ _: why of Just?"
  a@(VarExpr Maybe.Nothing _ varName)           ->
    if varName `elem` lv && varName `elem` funArgs then [Just $ JMLExpr a] else
    if varName `elem` lv && varName `notElem` funArgs then --throw $ NoteExcp $ printf "423:\n%s\t%s" varName (show lv)
      let (a@(AssignStmt _ (AssignExpr (VarExpr _ objs vn2) expr2)),typ) = getStmtOfVar stmts varName
      in if typ /= "For" then getEnsures extDeclList stmts funArgs lv expr2
         else [Just $ JMLExpr $ VarExpr Maybe.Nothing objs vn2]
    else throw $ NoteExcp "{{getEnsures}}: VarExpr Nothing _ varName: varName is not a local variable"
  ArrayExpr _ _                                 ->
    throw $ NoteExcp "{{getEnsures}}: ArrayExpr: un-implemented"
  BinOpExpr exprL binOp exprR                   ->
    let left  = getEnsures extDeclList stmts funArgs lv exprL
        right = getEnsures extDeclList stmts funArgs lv exprR
    in --if any isNothing (left++right) then throw $ NoteExcp [Maybe.Nothing] -- any (all isNothing) (left:right:[])
       {-else-} let extractExprL = map (\(Just (JMLExpr l))->l) (filter (/=Maybe.Nothing) left)--(filter (/=Nothing) left)
                    extractExprR = map (\(Just (JMLExpr r))->r) (filter (/=Maybe.Nothing) right)--(filter (/=Nothing) right)
            in zipWith (\l r -> Just $ JMLExpr $ BinOpExpr l binOp r) extractExprL extractExprR--[Just $ JMLExpr $ BinOpExpr ll binOp rr]

  UnOpExpr unOp expr                            ->
    let extractExpr = map (\(Just (JMLExpr x))->x) $ getEnsures extDeclList stmts funArgs lv expr
    in map (Just . JMLExpr . UnOpExpr unOp) extractExpr
  CondExpr{}                                    ->
    throw $ NoteExcp "{{getEnsures}}: CondExpr: un-implemented"
  AssignExpr _ expr                             ->
    getEnsures extDeclList stmts funArgs lv expr
  ExcpExpr _ _                                  -> [Maybe.Nothing]
  ReturnExpr Maybe.Nothing                      -> [Maybe.Nothing]
  ReturnExpr (Just expr)                        ->
    getEnsures extDeclList stmts funArgs lv expr

  where
    --deal with functions
    process1 :: [(Maybe Exception,JMLExpr,Maybe JMLExpr)] -> [ExternalDeclaration] -> String -> [Expression] -> [Maybe JMLExpr]
    process1 re _ _ _ = map (\(_,_,ens)->ens) re
     {- if isPure extDeclList funName
        && length re >= 1 then map (\(_,_,ens)->ens) re
      else throw $ NoteExcp $ printf "431:\n%s" (show re) -}

toJMLs :: Bool -> [[Expression]] -> [(Maybe Exception,JMLExpr,Maybe JMLExpr)] -> [JMLSyntax]
toJMLs whetherPure globalVAssignExpr list = zipWith (curry f) list [0 ..]
  where
    f :: ((Maybe Exception,JMLExpr,Maybe JMLExpr),Int) -> JMLSyntax
    f ((Maybe.Nothing,jml1,Just (JMLExpr jml2)),i) =
      let f a@(StringLiteral str) | "this." `isPrefixOf` str = VarExpr{varType = Maybe.Nothing,
                                                                       varObj = [],
                                                                       varName = tail $ dropWhile(/='.') str} --throw $ NoteExcp $ printf "\n___\n%s\n___\n" (show a)
          f a@BinOpExpr{} = BinOpExpr{expr1=f $ expr1 a,binOp = binOp a,expr2 = f $ expr2 a}
          f a = a
      in Normal_Behavior{requires=jml1,
                         assignable= if whetherPure then Assigned [] --when it's pure, then no global variable was assigned
                                     else if i < length globalVAssignExpr
                                            then Assigned (globalVAssignExpr !! i)
                                          else Assigned [], -- it's possible for a function to be impure with no newly assigned global variables
                         ensures = if i < length globalVAssignExpr
                                     then JMLExpr $ foldl' (`BinOpExpr` And) (f jml2) $ map (\a@AssignExpr{} ->
                                            BinOpExpr{expr1 = VarExpr{varType = varType $ assEleft a,
                                                                          varObj = varObj $ assEleft a,
                                                                          varName = {-"this." ++-} (varName $ assEleft a)},
                                                      binOp = Eq,
                                                      expr2 = assEright a}) (globalVAssignExpr !! i)
                                   else JMLExpr jml2}
    f ((Just excp,jml1,Maybe.Nothing),_) = Exceptional_Behavior{requires=jml1,
                                                                  signals=excp}

toJML :: [JMLSyntax] -> String
toJML = f "/*" where
  f :: String -> [JMLSyntax] -> String
  f res [] = res ++ "*/"
  f res [elm] = res ++ show elm ++ "*/"
  f res (elm:rest) = f (res ++ show elm ++ " also\n" ++ "  ") rest
