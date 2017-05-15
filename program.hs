import System.IO
import qualified Data.List


-- 2.1
type Ident = String

data Command a =
    Seq [Command a]
  | Copy Ident Ident
  | TAssign Ident (TExpr a)
  | CAssign Ident (CExpr a)
  | DeclareVector Ident (NExpr a)
  | Split Ident Ident Ident
  | Input Ident
  | Print (NExpr a)
  | Draw (TExpr a)
  | Pop Ident Ident
  | Push Ident Ident
  | Cond (BExpr a) (Command a) (Command a)
  | Loop (BExpr a) (Command a)
  deriving (Read, Eq)
  
data NExpr a =
    Var Ident
  | Const a
  | Plus (NExpr a) (NExpr a)
  | Minus (NExpr a) (NExpr a)
  | Times (NExpr a) (NExpr a)
  | Length Ident
  | Diameter Ident
  deriving (Read, Eq)

data BExpr a =
    And (BExpr a) (BExpr a)
  | Or (BExpr a) (BExpr a)
  | Not (BExpr a)
  | Gt (NExpr a) (NExpr a)
  | Lt (NExpr a) (NExpr a)
  | Eq (NExpr a) (NExpr a)
  | Empty Ident
  | Full Ident
  deriving (Read, Eq)
  
data TExpr a =
    TVar Ident
  | Merge (TExpr a) (CExpr a) (TExpr a)
  | Tube (NExpr a) (NExpr a)
  deriving(Read, Eq)
  
data CExpr a =
    CVar Ident
  | Connector (NExpr a)
  deriving(Read, Eq)
  

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
  
instance Show a => Show(BExpr a) where
  show (And x y) = (show x) ++ " AND " ++ (show y)
  show (Or x y) = (show x) ++ " OR " ++ (show y)
  show (Not x) = "NOT " ++ (show x)
  show (Gt x y) = (show x) ++ " > " ++ (show y)
  show (Lt x y) = (show x) ++ " < " ++ (show y)
  show (Eq x y) = (show x) ++ " = " ++ (show y)
  show (Empty x) = "EMPTY(" ++ x ++ ")"
  show (Full x) = "FULL(" ++ x ++ ")"

instance Show a => Show(NExpr a) where
  show (Var x) = x
  show (Const x) = show x
  show (Plus x y) =
    (show x) ++ " + " ++ (show y)
  show (Minus x y) =
    (show x) ++ " - " ++ (show y)
  show (Times x y) =
    (show x) ++ " * " ++ (show y)
  show (Length x) = "LENGTH(" ++ x ++ ")"
  show (Diameter x) = "DIAMETER(" ++ x ++ ")"
  
instance Show a => Show(TExpr a) where
  show (TVar x) = x
  show (Merge x y z) = "MERGE " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z)
  show (Tube x y) = "TUBE " ++ (show x) ++ " " ++ (show y)
  
instance Show a => Show(CExpr a) where
  show (CVar x) = x
  show (Connector x) = "CONNECTOR " ++ (show x)
  
  
-- 3.1
class SymTable m where
  update :: (Eq a) => (m a) -> String -> Val a -> (m a)
  value :: (Eq a) => (m a) -> String -> Maybe (Val a)
  start :: (Eq a) => (m a) -> (m a)
  
data Val a = 
    NumVal (NExpr a)
  | TubeVal (TExpr a)
  | ConnVal (CExpr a)
  | VecVal [(TExpr a)]
  deriving (Show, Eq)

-- 3.2
data Symbol a = Symbol Ident (Val a) deriving (Show, Eq)

getIdent :: Symbol a -> String
getIdent (Symbol ident _) = ident

getVal :: Symbol a -> Val a
getVal (Symbol _ val) = val


data SymTableList a = SymTableList [Symbol a] deriving(Show)

getTail :: SymTableList a -> [Symbol a]
getTail (SymTableList (x:xs)) = xs

instance SymTable SymTableList where
  update (SymTableList l@(x:xs)) ident val
    | (x:xs) == [] = SymTableList [(Symbol ident val)] 
    | (getIdent x) == ident = SymTableList ((Symbol ident val):xs)
    | xs == [] = SymTableList (x:[(Symbol ident val)])
    | otherwise = SymTableList (x: getTail (update (SymTableList xs) ident val))
  
  
  value (SymTableList l@(x:xs)) ident
    | l == [] = Nothing
    | (getIdent x) == ident = Just (getVal x)
    | xs == [] = Nothing
    | otherwise = value (SymTableList xs) ident
    
  start symt = symt

