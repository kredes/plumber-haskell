import System.IO

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
  | True
  | False
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
  start :: (Eq a) => (m a)
  
data Val a = 
    NumVal (NExpr a)
  | TubeVal (TExpr a)
  | ConnVal (CExpr a)
  | VecVal (NExpr a) [(TExpr a)]
  | BoolVal BExpr
  deriving (Show, Eq)

-- 3.2
data Symbol a = Symbol Ident (Val a) | None deriving (Show, Eq)

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
    
  start = SymTableList []

-- 3.3
data Tree a = Nil | Node (Tree a) a (Tree a) deriving(Show)

empty :: (Ord a) => Tree a -> Bool
empty Nil = True
empty  _  = False

contains :: (Ord a) => (Tree a) -> a -> Bool
contains Nil _ = False
contains (Node t1 v t2) x 
  | x == v = True
  | x  < v = contains t1 x 
  | x  > v = contains t2 x

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node t1 v t2) x 
  | v == x = Node t1 v t2
  | v  < x = Node t1 v (insert t2 x)
  | v  > x = Node (insert t1 x) v t2

delete :: (Ord a) => Tree a -> a -> Tree a
delete Nil _ = Nil
delete (Node t1 v t2) x  
  | x == v = deleteX (Node t1 v t2)
  | x  < v = Node (delete t1 x) v t2
  | x  > v = Node t1 v (delete t2 x)

-- Delete root
deleteX :: (Ord a) => Tree a -> Tree a 
deleteX (Node Nil v t2) = t2
deleteX (Node t1 v Nil) = t1
deleteX (Node t1 v t2) = (Node t1 v2 (delete t2 v2))
  where 
    v2 = leftmostElement t2

-- Return leftmost element of tree
leftmostElement :: (Ord a) => Tree a -> a
leftmostElement (Node Nil v _) = v
leftmostElement (Node t1 _ _) = leftmostElement t1


data SymTableTree a = SymTableTree (Tree (Symbol a)) deriving(Show)

getT1 (SymTableTree (Node t1 _ _)) = t1
getT2 (SymTableTree (Node _ _ t2)) = t2

getTree (SymTableTree node) = node


instance SymTable SymTableTree where
  update (SymTableTree Nil) ident val = SymTableTree (Node Nil (Symbol ident val) Nil)
  update (SymTableTree t@(Node t1 v t2)) ident val
    | v == None = SymTableTree (Node Nil (Symbol ident val) Nil)
    | idv == ident = SymTableTree (Node t1 (Symbol ident val) t2)
    | ident < idv = SymTableTree (Node (getTree (update (SymTableTree t1) ident val)) v t2)
    | ident > idv = SymTableTree (Node t1 v (getTree (update (SymTableTree t2) ident val)))
    where 
      idv = getIdent v
  
  value (SymTableTree Nil) ident = Nothing
  value (SymTableTree t@(Node t1 v t2)) ident
    | idv == ident = Just (getVal v)
    | ident < idv = value (SymTableTree t1) ident
    | ident > idv = value (SymTableTree t2) ident
    where
      idv = getIdent v
    
  start = SymTableTree (Node Nil None Nil)

-- 3.4
left (Left a) = a
right (Right b) = b

class Evaluable e where
  eval :: (Num a, Ord a, SymTable m) => m a -> (e a) -> (Either String (Val a))
  --typeCheck :: (Ident -> String) -> (e a) -> Bool
  
getLength (Just (TubeVal (Tube l _))) =  l
getDiameter (Just (TubeVal (Tube _ d))) = d
getNum (Just (NumVal n)) = n

  
instance Evaluable NExpr where
  eval symt (Var ident) =
    if (value symt ident) == Nothing
       then Left "error: undefined variable" 
       else eval symt (getNum (value symt ident))
  eval symt (Const n) = Right n
  eval symt (Plus x y) = Right $ NumVal $ Const $ (right $ eval symt x) + (right $ eval symt y)
  eval symt (Minus x y) = Right $ NumVal $ Const $ (right $ eval symt x) - (right $ eval symt y)
  eval symt (Times x y) = Right $ NumVal $ Const $ (right $ eval symt x) * (right $ eval symt y)
  eval symt (Length ident) = 
    if (value symt ident) == Nothing 
       then Left "error: undefined variable" 
       else eval symt (getLength (value symt ident))
  eval symt (Diameter ident) =
    if (value symt ident) == Nothing
       then Left "error: undefined variable"
       else eval symt (getDiameter (value symt ident))
       

getVecSize (Just (VecVal _ elems)) = length elems
getVecMaxSize (Just (VecVal maxSize _)) = maxSize
       
instance Evaluable BExpr where
  eval symt (And x y) = Right $ (right $ eval symt x) && (right $ eval symt y)
  eval symt (Or x y) = Right $ (right $ eval symt x) || (right $ eval symt y)
  eval symt (Not x) = Right $ not (right $ eval symt x)
  eval symt (Gt x y) = Right $ (right $ eval symt x) > (right $ eval symt y)
  eval symt (Lt x y) = Right $ (right $ eval symt x) < (right $ eval symt y)
  eval symt (Eq x y) = Right $ (right $ eval symt x) == (right $ eval symt y)
  eval symt (Empty ident) = 
    if (value symt ident) == Nothing
       then Left "error: undefined variable" 
       else Right $ BoolVal (getVecSize (value symt ident) == 0)
  eval symt (Full ident) = 
    if (vec) == Nothing
       then Left "error: undefined variable" 
       else Right $ BoolVal ((getVecSize vec) == (right $ eval symt (getVecMaxSize vec)))
    where vec = value symt ident
  
data TExpr a =
    TVar Ident
  | Merge (TExpr a) (CExpr a) (TExpr a)
  | Tube (NExpr a) (NExpr a)
  deriving(Read, Eq)
  
data CExpr a =
    CVar Ident
  | Connector (NExpr a)
  deriving(Read, Eq)
  
  
instance Evaluable TExpr where
  eval symt (TVar ident) = 
    if (value symt ident) == Nothing
       then Left "error: undefined variable" 
       else Right $ getVecSize (value symt ident) == 0
  
--interpretCommand :: (Num a, Ord a, SymTable m) => m a -> [a] -> Command a -> ((Either String [a]), m a, [a])