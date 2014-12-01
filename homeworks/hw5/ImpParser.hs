module ImpParser where

import ImpSyntax
import Text.Parsec
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Expr as E
import System.Environment
import Control.Monad

--
-- Parser for the "Imp" language.
--
-- To parse a file, you can say the following at the ghci prompt:
--
--    > parseImp "~/fac.imp"
--
-- and this will print out an abstract syntax tree.
--
-- You can also save the abstract syntax tree. For example:
--
--    > ast <- parseImp "~/fac.imp"
--
-- will parse the file ~/fac.imp and store the abstract syntax tree in the
-- variable "ast" which can be referred to later in your ghci session.
--
lang :: L.LanguageDef st
lang = L.emptyDef { T.commentStart    = "/*",
                    T.commentEnd      = "*/",
                    T.commentLine     = "//",
                    T.nestedComments  = True,
                    T.identStart      = letter,
                    T.identLetter     = letter <|> char '_' <|> digit,
                    T.opStart         = foldr1 (<|>) (map char "+-*=><&|!:"),
                    T.opLetter        = foldr1 (<|>) (map char "=&|"),
                    T.reservedNames   = ["if","else","while","true","false", "in","case","for","function","return","put"],
                    T.reservedOpNames = ["+","-","*","==","!=",">","<",">=","<=","&&","||","!"],
                    T.caseSensitive   = True }

p = T.makeTokenParser lang

identifier = T.identifier p
reservedOp = T.reservedOp p
semi       = T.semi p
comma      = T.comma p
braces     = T.braces p
reserved   = T.reserved p
parens     = T.parens p
whiteSpace = T.whiteSpace p
integer    = T.integer p

-- programs
prog = do -- fs <- many fundefn
          ss <- many stmt
          --return (fs,ss)
          return ss
           
-- statements
assign_nosemi = do i <- identifier
                   reservedOp ":="
                   e <- expr
                   return (Assign i e)

assign = do i <- identifier
            reservedOp ":="
            e <- expr
            semi
            return (Assign i e)

case_ = do reserved "case"
           e    <- expr
           arms <- braces (many caseArm)
           return (Case e arms)
if_   = do reserved "if"
           be    <- bexpr
           ss_t  <- braces (many stmt)
           melse <- optionMaybe (reserved "else")
           case melse of
             Nothing -> return (If be ss_t [])
             Just () -> do ss_f <- braces (many stmt)
                           return (If be ss_t ss_f)
while = do reserved "while"
           be   <- bexpr
           ss_t <- braces (many stmt)
           return (While be ss_t)
for =  do reserved "for"
          (s1,e,s2) <- parens (do
                                  s1 <- assign
                                  e  <- bexpr
                                  semi
                                  s2 <- assign_nosemi
                                  return (s1,e,s2)
                              )
          ss <- braces (many stmt)
          return $ For s1 e s2 ss

stmt = assign
   <|> case_
   <|> if_
   <|> while
   <|> for

caseArm = do n  <- integer
             ss <- braces (many stmt)
             return (fromInteger n,ss)
        
-- operator table
optable = [[E.Prefix (do { reservedOp "-" ; return Neg })],
           
           [E.Infix (do { reservedOp "*" ; return Mul }) E.AssocLeft],
           
           [E.Infix (do { reservedOp "+" ; return Add }) E.AssocLeft,
            E.Infix (do { reservedOp "-" ; return Sub }) E.AssocLeft]
          ]

-- terms
term = parens expr
   <|> liftM (LitInt . fromInteger) integer
   <|> (do i     <- identifier
           return (Var i))
           --margs <- optionMaybe (parens (expr `sepBy` comma))
           --case margs of
           --  Nothing   -> return (Var i)
           --  Just args -> return (FunCall i args))

-- expressions
expr = E.buildExpressionParser optable term

-- operator table for bexps
boptable = [[E.Infix (do { reservedOp "&&" ; return And }) E.AssocLeft],
            [E.Infix (do { reservedOp "||" ; return Or }) E.AssocLeft]
          ]

-- comparison operators for bexps
comparisonOperator = (reservedOp ">" >> return IsGT)
                 <|> (reservedOp "<" >> return IsLT)
                 <|> (reservedOp ">=" >> return IsGTE)
                 <|> (reservedOp "<=" >> return IsLTE)
                 <|> (reservedOp "==" >> return IsEq)
                 <|> (reservedOp "!=" >> return IsNEq)    

-- terms for bexps
bterm = (reservedOp "!" >> bterm0 >>= \ bt -> return (Not bt))
    <|> bterm0
    <|> (do e  <- expr
            o  <- comparisonOperator
            e' <- expr
            return (o e e'))
   
bterm0 = parens bexpr
     <|> (reserved "true" >> return (LitBool True))
     <|> (reserved "false" >> return (LitBool False))

bexpr = E.buildExpressionParser boptable bterm

-- utility function to expand ~ in filenames
expandFilePath :: FilePath -> IO FilePath
expandFilePath ('~':'/':s) = do hd <- getEnv "HOME"
                                return (hd ++ "/" ++ s)
expandFilePath s           = return s

-- main entry points for parser
parseImp :: FilePath -> IO Prog
parseImp p_ = do p      <- expandFilePath p_
                 s      <- readFile p
                 let pr =  runParser (whiteSpace >> prog >>= \ pg -> whiteSpace >> eof >> return pg) () p s
                 case pr of
                   Left err -> fail (show err)
                   Right ss -> return ss

parseFile :: FilePath -> IO Prog
parseFile = parseImp

parseProg :: String -> Prog
parseProg s = case runParser (whiteSpace >> prog >>= \ pg -> whiteSpace >> eof >> return pg) () "<no filename>" s of
                Left err -> error (show err)
                Right ss -> ss

parseStmt :: String -> Stmt
parseStmt s = case runParser (whiteSpace >> stmt >>= \ st -> whiteSpace >> eof >> return st) () "<no filename>" s of
                Left err -> error (show err)
                Right st -> st

parseExp :: String -> Exp
parseExp s = case runParser (whiteSpace >> expr >>= \ e -> whiteSpace >> eof >> return e) () "<no filename>" s of
               Left err -> error (show err)
               Right e  -> e

parseBExp :: String -> BExp
parseBExp s = case runParser (whiteSpace >> bexpr >>= \ e -> whiteSpace >> eof >> return e) () "<no filename>" s of
                Left err -> error (show err)
                Right e  -> e
