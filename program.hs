import System.IO
import System.Random


-- 2.1
type Ident = String

data Command a =
    Seq [Command a]
  | Copy Ident Ident
  | TAssign Ident TExpr
  | CAssign Ident Connector
  | DeclareVector Ident NExpr
  | Split Ident Ident Ident
  | Input Ident
  | Print NExpr
  | Draw TExpr
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
  

-- 2.2 - Shows a command with the given indentation (s)
showind :: Show a => Command a -> String -> String
showind (Seq []) s    = ""
showind (Seq (x:xs)) s  =
  (showind x s) ++ "\n" ++ (showind (Seq xs) s)

showind (Cond be c1 (Seq [])) s =
  s ++ "IF " ++ (show be) ++ " THEN\n" ++
  (showind c1 ("  " ++ s)) ++ s ++ "END\n"

showind (Cond be c1 c2) s =
  s ++ "IF " ++ (show be) ++ " THEN\n" ++
  (showind c1 ("  " ++ s)) ++ s ++ "ELSE\n" ++
  (showind c2 ("  " ++ s)) ++ s ++ "END"

showind (TAssign x y) s = s ++ x ++ " = " ++ (show y)
showind (CAssign x y) s = s ++ x ++ " = " ++ (show y)
showind (DeclareVector x y) s = s ++ x ++ " = TUBEVECTOR OF " ++ (show y)
showind (Split x y z) s = s ++ "(" ++ x ++ ", " ++ y ++ ") = SPLIT " ++ z

showind (Input x) s = s ++ "INPUT " ++ x
showind (Print x) s = s ++ "PRINT " ++ (show x)
showind (Draw x) s = s ++ (show x)

showind (Pop x y) s = s ++ "POP " ++ x ++ " " ++ y
showind (Push x y) s = s ++ "PUSH " ++ x ++ " " ++ y
showind (Loop be c1) s =
  s ++ "WHILE " ++ (show be) ++ "\n" ++ s ++ "DO\n" ++
  (showind c1 ("  " ++ s)) ++ s ++ "END"
    
instance Show a => Show(Command a) where
  show x = showind x ""
  
