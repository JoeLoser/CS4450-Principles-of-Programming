-- Joe Loser
-- CS4450 HW5
-- 11/30/14

module ImpInterp where
import Prelude hiding (exp)
import ImpSyntax
import ImpParser (parseImp, parseProg)

-----------------------------------
--    Environment Operations     --
-----------------------------------

type Env        = (Name -> Int)

tweek :: Name -> Int -> Env -> Env
tweek x v r = \ y -> if x == y 
                      then v 
                      else r y

-----------------------------------
--    Expression Semantics       --
-----------------------------------

exp :: Exp -> Env -> Int 
exp (Add e1 e2) env      = (exp e1 env) + (exp e2 env)
exp (Sub e1 e2) env      = (exp e1 env) - (exp e2 env)
exp (Mul e1 e2) env      = (exp e1 env) * (exp e2 env) 
exp (Neg e) env          = -(exp e env)
exp (Var x) r            = r x
exp (LitInt m) _         = m 

-----------------------------------
--      Boolean Semantics        --
-----------------------------------

bool :: BExp -> Env ->  Bool
bool (IsEq  e1 e2) r = exp e1 r == exp e2 r       
bool (IsNEq e1 e2) r = not (exp e1 r == exp e2 r)
bool (IsGT  e1 e2) r = exp e1 r >  exp e2 r 
bool (IsGTE e1 e2) r = exp e1 r >=  exp e2 r 
bool (IsLT e1 e2)  r = exp e1 r < exp e2 r 
bool (IsLTE e1 e2) r = exp e1 r <= exp e2 r 
bool (And   b1 b2) r = bool b1 r &&  bool b2 r 
bool (Or    b1 b2) r = bool b1 r || bool b2 r 
bool (Not b)       r = not (bool b r )          
bool (LitBool x)  _  = x          

-----------------------------------
--     Statement Semantics       --
-----------------------------------

stmt :: Stmt -> Env -> Env 
stmt (Assign x e) r = tweek x (exp e r) r
stmt (If b pt pf) r = if (bool b r) 
                          then (stmts pt r) 
                          else (stmts pf r) 
stmt (While b p) r  = if (bool b r) 
                          then (stmts (p++[While b p]) r) 
                          else r 

-----------------------------------
--  Statement List Semantics     --
-----------------------------------

stmts :: [Stmt] -> Env -> Env
stmts [] r       = r 
stmts (st:[]) r  = stmt st r 
stmts (st:sts) r = stmts sts (stmt st r)

-----------------------------------
-- Program & Function Semantics  --
-----------------------------------

prog :: [Stmt] -> Env
prog body = stmts body r0
  where
    r0  = (\ n -> error ("Unbound: " ++ n))

-- The interpreter is defined as a transformation from files to an environment.
-- The meaning of your program depends upon what meaning you assign to your
-- variables.
interp :: FilePath -> IO Env
interp fn = do p <- parseImp fn
               let result = (prog p)
               return result

-- This function executes an Imp program and assumes that the output value will
-- be stored in a variable called "result".  Programs in examples/ serve as,
-- well, examples. For instance, try: test_program "examples/factorial6.imp" to
-- test.
test_program :: FilePath -> IO ()
test_program f = do
                   env <- interp f
                   print $ env "result"
