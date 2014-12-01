module ImpSyntax where

--
-- Abstract syntax for the "Imp" language.
--

-- names (variables) are just strings.
type Name = String

-- a program is a series (list) of function definitions, followed by a
-- series of statements.
type Prog = [Stmt]

-- a statement is either...
data Stmt =
    Assign Name Exp         -- ...assignment (<name> := <exp>;)
  | If BExp [Stmt] [Stmt]   -- ...if-then-else (if <bexp> { <stmt>* } else { <stmt>* })
  | While BExp [Stmt]       -- ...or a while-loop (while <bexp> { <stmt>*> })
  | Case Exp [(Int,[Stmt])] -- ...case statements
  | For Stmt BExp Stmt [Stmt] -- ...for statements 
  deriving (Eq, Show)

-- an integer expression is either...
data Exp =
    Add Exp Exp             -- ...addition (<exp> + <exp>)
  | Sub Exp Exp             -- ...subtract (<exp> - <exp>)
  | Mul Exp Exp             -- ...multiplication (<exp> * <exp>)
  | Neg Exp                 -- ...negation (-<exp>)
  | Var Name                -- ...a variable (<name>)
  | LitInt Int              -- ...or an integer literal (e.g. 3, 0, 42, 1999)
  -- | FunCall Name [Exp]      -- ...or a function call (<name> (<exp>,...,<exp))
  deriving (Eq, Show)

-- a boolean expression is either...
data BExp =
    IsEq Exp Exp            -- ...test for equality (<exp> == <exp>)
  | IsNEq Exp Exp           -- ...test for inequality (<exp> != <exp>)
  | IsGT Exp Exp            -- ...test for greater-than (<exp> > <exp>)
  | IsLT Exp Exp            -- ...test for less-than (<exp> < <exp>)
  | IsGTE Exp Exp           -- ...test for greater-or-equal (<exp> >= <exp>)
  | IsLTE Exp Exp           -- ...test for less-or-equal (<exp> <= <exp>)
  | And BExp BExp           -- ...boolean and (<bexp> && <bexp>)
  | Or BExp BExp            -- ...boolean or (<bexp> || <bexp>)
  | Not BExp                -- ...boolean negation (!<bexp>)
  | LitBool Bool            -- ... or a boolean literal (true or false)
  deriving (Eq, Show)
