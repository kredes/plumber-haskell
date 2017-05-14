import System.IO
import System.Random

type Ident = String

data Command a =
    Seq [Command a]
  | Copy Ident Ident
  | TAssign Ident Tube
  | CAssign Ident Connector
  | DeclareVector Ident NExpr
  | Split Ident Ident Ident
  | Input Ident
  | Print NExpr
  | Draw Ident
  | Pop Ident Ident
  | Push Ident Tube
  | Cond (BExpr a) (Command a) (Command a)
  | Loop (BExpr a) (Command a)
  deriving (Read)
  
data NExpr a =
    Var Ident
  | Const a
  | Length Ident
  | Plus (NExpr a) (NExpr a)
  | Minus (NExpr a) (NExpr a)
  | Times (NExpr a) (NExpr a)
  | Length Ident
  | Diameter Ident
  deriving (Read)

data BExpr a =
    And (BExpr a) (BExpr a)
  | Or (BExpr a) (BExpr a)
  | Not (BExpr a)
  | Gt (NExpr a) (NExpr a)
  | Lt (NExpr a) (NExpr a)
  | Eq (NExpr a) (NExpr a)
  | Empty Ident
  | Full Ident
  deriving (Read)
  
data TExpr a =
    TVar Ident
  | Merge (TExpr a) (CExpr a) (TExpr a)
  | Tube (Nexpr a) (NExpr a)
  
data CExpr a =
    CVar Ident
  | Connector (NExpr a)
    
