-- Joe Loser
-- CS 4450 Homework 5
-- 12/5/2014

module ImpInterp where

import Prelude hiding (exp)
import ImpSyntax
import ImpParser hiding (stmt,prog)
import Data.List -- for intercalate

-----------------------------------
--      Addresses and Values     --
-----------------------------------

type Address    = Int
data Value      = Number Int 
                | Loc Address 
                | Nil
                | FunVal (Env -> State -> [Value] -> Value)

instance Show Value where
  show (Number i) = show i
  show (FunVal _) = "Function Value not Showable"

instance Eq Value where
  Number i == Number j = i==j
  _ == _ = error "Num eqs compared to vals" --undefined

instance Ord Value where
  Number i <= Number j = i <= j 
  _ <= _ = undefined
  Number i >= Number j = i >= j
  _ >= _ = undefined


--
-- How would you define Value arithmetic?
--

instance Num Value where
  Number i + Number j = Number (i+j)
  Number i + Loc j    = Loc (i+j)
  Loc i + Number j    = Loc (i+j)
  Loc i + Loc j       = Loc (i+j)
  Number i * Number j = Number (i*j)
  Number i - Number j = Number (i-j)
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

-----------------------------------
--    Environment Operations     --
-----------------------------------

type Env        = Name -> Value

tweek x v r = \ y -> if x==y then v else r y

extend :: Env -> [(Name,Value)] -> Env
extend r []         = r
extend r ((x,v):bs) = tweek x v (extend r bs)

-----------------------------------
--        State Operations       --
-----------------------------------

type State = (Address,Address -> Value)

alloc :: State -> State
alloc (a,s) = (a+1,s)

-- assigns a value to an address in a State
update :: Address -> Value -> State -> State 
update x v (a,s) = (a,s')
    where
       s' = tweek x v s

-- 1. Look up value bound to x
-- 2. If it's a number, return it.
-- 3. If it's an address, look up the contents in the state.
varread :: Name -> Env -> State -> Value
varread x r (_,s) = case (r x) of
                         Number i -> Number i
                         Loc a    -> s a

--
varstore :: Name -> Value -> Env -> State -> State
varstore x v r s = case r x of
                        Number i -> error "argh!"
                        Loc a    -> update a v s
    
-----------------------------------
--    Expression Semantics       --
-----------------------------------

exp :: Exp -> Env -> State -> Value
exp (Add e1 e2) r s    = (exp e1 r s) + (exp e2 r s)
exp (Sub e1 e2) r s    = (exp e1 r s) - (exp e2 r s)
exp (Mul e1 e2) r s    = (exp e1 r s) * (exp e2 r s)
exp (Neg e) r s        = -(exp e r s)
exp (Var x) r s        = varread x r s
exp (LitInt m) _ _     = Number m 
exp (FunCall f xs) r s = fv r s vs
    where 
       FunVal fv = r f
       vs        = map (\ e -> exp e r s) xs


-----------------------------------
--      Boolean Semantics        --
-----------------------------------

bool :: BExp -> Env -> State -> Bool
bool (IsEq  e1 e2) r s = exp e1 r s == exp e2 r s       
bool (IsNEq e1 e2) r s = not(exp e1 r s == exp e2 r s)
bool (IsGT  e1 e2) r s = exp e1 r s >  exp e2 r s
bool (IsGTE e1 e2) r s = exp e1 r s >=  exp e2 r s
bool (IsLT e1 e2)  r s = exp e1 r s < exp e2 r s
bool (IsLTE e1 e2) r s = exp e1 r s <= exp e2 r s
bool (And   b1 b2) r s = bool b1 r s &&  bool b2 r s
bool (Or    b1 b2) r s = bool b1 r s || bool b2 r s
bool (Not b)       r s = not (bool b r s)          
bool (LitBool x)  _  _  = x          

-----------------------------------
--     Statement Semantics       --
-----------------------------------

--stmt :: Stmt -> Env -> State -> State
stmt :: Stmt -> Env -> State -> (Value, State)
stmt (Assign x e) r s = (Nil, varstore x (exp e r s) r s)
stmt (If b pt pf) r s = if (bool b r s) 
                          then (stmts pt r s) 
                          else (stmts pf r s) 
stmt (While b p) r s  = if (bool b r s) 
                          then (stmts (p++[While b p]) r s) 
                          else (Nil, s)
stmt (Let x e p) r s  = stmts p r' s''
                          where (ax,s') = alloc s
                                r'      = tweek x (Loc ax) r
                                s''     = update ax v (ax,s')
                                v       = exp e r s
stmt (Return e) r s   = (v,s)
    where 
      v = exp e r s

-- Added functionality for For loops
stmt (For (Assign n e) b st2 ss) r s = 
    let x = While b (ss ++ [st2])
    in stmt (Let n e [x]) r s

-- Added functionality for Case statements
-- Recall that the following is for Case statements: Case Exp [(Int,[Stmt])]
-- Let's decompose the tuple (pair) which is (Int, [Stmt])
stmt (Case e (pair:pairs)) r s = 
  if (x == key) then (stmts statements r s)
  else (stmt (Case e pairs) r s) -- run through the rest of the pairs via recursion
  where 
  key = fst pair -- gets me the key I want to access for my array
  statements = snd pair -- gets me the list of statements
  Number x = exp e r s -- gets me the number I want to be looking up in my array of key-statements

-----------------------------------
--  Statement List Semantics--
-----------------------------------

--stmts :: [Stmt] -> Env -> State -> State
stmts :: [Stmt] -> Env -> State -> (Value, State)
stmts [] _ s       = (Nil,s)
stmts (Return e : sts) r s = stmt (Return e) r s
stmts (st:[]) r s = stmt st r s
stmts (st:sts) r s = stmts sts r (snd (stmt st r s))

-----------------------------------
-- Program & Function Semantics  --
-----------------------------------
-- Recall that 
--type Prog = ([FunDefn],[Stmt]) -- list of functions and their makeup, and the list of statements to execute afterwards
--type FunDefn = (Name,[Name],[Stmt]) -- i.e. the name of the function, list of parameters, and the body of the function

-- Added functionality for prettyPrint which maps over the function definitions and statements and concatenates them together to form a String
prettyPrint ::  Prog -> String
prettyPrint (functionDefinitions, statements) = 
  parseFunctionDefs functionDefinitions ++ parseStatements statements


parseFunctionDefs :: [FunDefn] -> String
parseFunctionDefs = concatMap parseFunctionDef

-- Helper function to parse FunDefn and turn them into a String by decomposing the tuple and working with that
parseFunctionDef :: FunDefn -> String
parseFunctionDef (nameOfFunction, parameterList, body) = 
  "function " ++ nameOfFunction ++ "(" ++ parseParameters parameterList ++ "){" ++ parseStatements body ++ "}"

-- Helper function used to concatenate a list of Strings split by comma
parseParameters :: [Name] -> String
parseParameters params = intercalate "," params

parseStatements :: [Stmt] -> String
parseStatements = concatMap parseStatement

-- Main part of prettyPrint -- will parse a statement and build a String out of it
-- parseStatement will manually add the semi colons for the statements that need them (i.e. Assign  and Return)
parseStatement :: Stmt -> String
parseStatement (Assign name e) = name ++ ":=" ++ parseExpression e ++ ";"
  -- Can just always assume else statement(s) since could be empty and we won't have to worry :D
  -- Need to add the semi colon at the end of each statement since intercalate only covers the semi
  -- colons between the list of statements
parseStatement (If x y z) = 
  "if(" ++ parseBinaryExp x ++ "){" ++ parseStatements y ++ "}else{" ++ parseStatements z ++ "}"
parseStatement (While b ss) = "while(" ++ parseBinaryExp b ++ "){" ++ parseStatements ss ++ "}"
parseStatement (Let n e ss) = "let " ++ n ++ ":= " ++ parseExpression e ++ "in{" ++ parseStatements ss ++ "}"
parseStatement (Case e cases) = "case " ++ parseExpression e ++ "{" ++ (concatMap parseCase cases) ++ "}"
-- Need to use init on the last statement since I manually add semi colons and I do not want a semi colon for the last statement in for loop
-- since it is not valid syntax then
-- I should probably make sure that the statements s1 and s2 are really Assign statements otherwise throw an Error, but I am putting it on the
-- user. If they're using my Interp, surely they know how to write a for loop :)
parseStatement (For s1 b s2 ss) =  "for (" ++ parseFor s1 b s2 ++ "){" ++ parseStatements ss ++ "}"
parseStatement (Return e) = "return " ++ parseExpression e ++ ";"

parseFor :: Stmt -> BExp -> Stmt -> String
parseFor s1 b s2 = parseStatement s1 ++ parseBinaryExp b ++ ";" ++ init (parseStatement s2)

-- Used to help parse Case statements
parseCase :: (Int, [Stmt]) -> String
parseCase (x, ys) = show x ++ "{" ++ parseStatements ys ++ "}"

-- Helper function used to parse BExp
parseBinaryExp :: BExp -> String
parseBinaryExp (IsEq x y) = parseExpression x ++ "==" ++ parseExpression y
parseBinaryExp (IsNEq x y) = parseExpression x ++ "!=" ++ parseExpression y
parseBinaryExp (IsGT x y) = parseExpression x ++ ">" ++ parseExpression y
parseBinaryExp (IsLT x y) = parseExpression x ++ "<" ++ parseExpression y
parseBinaryExp (IsGTE x y) = parseExpression x ++ ">=" ++ parseExpression y
parseBinaryExp (IsLTE x y) = parseExpression x ++ "<=" ++ parseExpression y
parseBinaryExp (And x y) = parseBinaryExp x ++ "&&" ++ parseBinaryExp y
parseBinaryExp (Or x y) = parseBinaryExp x ++ "||" ++ parseBinaryExp y
parseBinaryExp (Not x) = "!" ++ "(" ++ parseBinaryExp x ++ ")"
parseBinaryExp (LitBool b) = if b then "true" else "false"

-- Helper function to parse Exp
parseExpression :: Exp -> String
parseExpression (Add x y) = parseExpression x ++ "+" ++ parseExpression y
parseExpression (Sub x y) = parseExpression x ++ "-" ++ parseExpression y
parseExpression (Mul x y) = parseExpression x ++ "*" ++ parseExpression y
parseExpression (Neg x) = "-" ++ parseExpression x
parseExpression (Var x) = x
parseExpression (LitInt x) = show x
parseExpression (FunCall x exps) = x ++ "(" ++ parseExpList exps ++ ")"

-- Helper function to parse a list of Exp
parseExpList :: [Exp] -> String
parseExpList exps = intercalate "," (map parseExpression exps)

fundef :: FunDefn -> Env -> State -> ([Value] -> Value)
fundef (n,xs,ps) r s vs = v
     where
       (v,_) = stmts ps r' s
       r'    = extend r (zip xs vs)

prog (fdecls,body) = stmts body r0 s0
  where
    fds = map (FunVal . fundef) fdecls
    fs  = map fst3 fdecls
           where fst3 (x,_,_) = x
    r0  = extend (\ n -> error ("Unbound: " ++ n)) (zip fs fds)
    s0  = (0, \ a -> error ("Unititialized Location: " ++ show a))

interp fn = do p <- parseImp fn
               let result = fst (prog p)
               print result

-----------------------------------
--          Examples             --
-----------------------------------

dbl :: FunDefn
dbl = ("double", ["m"], [Return (Add (Var "m")  (Var "m"))])

ad :: FunDefn
ad = ("add",["m","n"], [Return (Add (Var "m")  (Var "n"))]) 

factorial :: FunDefn
factorial = ("fac",["n"],[If (IsEq (Var "n") (LitInt 0)) [Return (LitInt 1)] [Return (Mul (Var "n") (FunCall "fac" [Sub (Var "n") (LitInt 1)]))]])
