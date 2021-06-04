{-# Language LambdaCase #-}
module Parser.Types where

import qualified Control.Exception as E

data BinOp = Plus | Mult | Minus | Div | Mod | Less | LessEq | Greater | GreaterEq | Eq | Neq | And | Or deriving Eq
instance Show BinOp where
  show Plus      = "+"
  show Mult      = "*"
  show Minus     = "-"
  show Div       = "/"
  show Mod       = "%"
  show Less      = "<"
  show LessEq    = "<="
  show Greater   = ">"
  show GreaterEq = ">="
  show Eq        = "=="
  show Neq       = "!="
  show And       = "&&"
  show Or        = "||"

-- | precedence of binary operators in proper order
data Prec = PMul | PAdd | PCmp | PAnd | POr deriving (Enum, Eq, Ord, Show)

prec :: BinOp -> Prec
prec = \case
  Plus -> PAdd
  Mult -> PMul
  Minus -> PAdd
  Div -> PMul
  Mod -> PMul
  And -> PAnd
  Or -> POr
  _ -> PCmp

-- | associativity of binary operators
data Assoc = ALeft | ARight | ANon deriving (Eq, Show)

-- | compute associativity from precedence rather than from operators directly
assoc :: Prec -> Assoc
assoc = \case
  PMul -> ALeft
  PAdd -> ALeft
  PCmp -> ANon
  _ -> ARight

data UnOp = NotOp deriving Eq
instance Show UnOp where
  show NotOp = "!"

data Expression
  = IntLiteral Int
  | BoolLiteral Bool
  | CharLiteral Char
  | StringLiteral String
  | Null
  | VarExpr {varType :: Maybe (Type Types), varObj :: [String], varName :: String}
  | ArrayExpr {arrName :: Expression, index :: Maybe Expression}
  | BinOpExpr {expr1 :: Expression, binOp :: BinOp, expr2 :: Expression}
  | UnOpExpr {unOp :: UnOp, expr :: Expression}
  | FunCallExpr {funName :: Expression, funArgs :: [Expression]}
  | CondExpr {eiff :: Expression, ethenn :: Expression, eelsee :: Expression}
  | AssignExpr {assEleft :: Expression, assEright :: Expression}
  | ExcpExpr {excpName :: Exception, excpmsg :: Maybe String}
  | ReturnExpr {returnE :: Maybe Expression}
  deriving (Eq, Show)

isLiteral :: Expression -> Bool
isLiteral (IntLiteral _) = True
isLiteral (BoolLiteral _) = True
isLiteral (CharLiteral _) = True
isLiteral (StringLiteral _) = True
isLiteral _ = False

fromBoolLiteral :: Expression -> Bool
fromBoolLiteral (BoolLiteral bool) = bool
fromBoolLiteral _ = undefined

data Statement
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
  deriving (Eq, Show)

data ExternalDeclaration = FunDef {funModifier :: [Modifier],
                                   isPureFlag :: Bool,
                                   funDecl :: Statement,
                                   throws :: Maybe Exception,
                                   funBody :: Statement} deriving(Eq,Show)

data Type a
  = BuiltInType a
  | AnyType {typee :: String, generic :: Maybe (Type a)}
  | ArrayType {baseType :: Type a}
  deriving (Eq, Show)

data Types
  = Int
  | Void
  | Char
  | String
  | Boolean
  | Double
  | Short
  | Float
  | Long
  | Byte

--  | Exception Exception
  deriving(Eq,Show)

data Modifier
  = Static
  | Public
  | Private
  | Protected
  | Final
  | Abstract
  deriving (Eq, Show)

-- | https://www.geeksforgeeks.org/types-of-exception-in-java-with-examples/
newtype Exception = Exception String deriving Eq
instance Show Exception where
  show (Exception str) = str

newtype NotatedException = NoteExcp String

instance Show NotatedException where
  show (NoteExcp str) = str

instance E.Exception NotatedException
