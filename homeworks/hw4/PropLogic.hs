module PropLogic(Var, Prop(..), parse) where

import Parsing (Parser(..), symbol, identifier, (+++))

type Var = String

data Prop = Atom Var
          | Not Prop
          | Imply Prop Prop
          | Or Prop Prop
          | And Prop Prop
          | Iff Prop Prop
          deriving (Eq)

instance Show Prop where
  show (Atom p)            = p
  show (Not prop)          = "(-" ++ show prop ++ ")"
  show (Imply prop1 prop2) = "(" ++ show prop1 ++ " => " ++ show prop2 ++ ")"
  show (Or prop1 prop2) = "(" ++ show prop1 ++ " \\/ " ++ show prop2 ++ ")"
  show (And prop1 prop2) = "(" ++ show prop1 ++ " /\\ " ++ show prop2 ++ ")"
  show (Iff prop1 prop2) = "(" ++ show prop1 ++ " <=> " ++ show prop2 ++ ")"

parseAtom :: Parser Prop
parseAtom  = do x <- identifier
                return (Atom x)

parseImply :: Parser Prop
parseImply = do symbol "("
                wf1 <- parsePL
                symbol "=>"
                wf2 <- parsePL
                symbol ")"
                return (Imply wf1 wf2)

parseNot :: Parser Prop
parseNot   = do symbol "("
                symbol "-"
                wf <- parsePL
                symbol ")"
                return (Not wf)

parseOr :: Parser Prop
parseOr    = do symbol "("
                wf1 <- parsePL
                symbol "\\/"
                wf2 <- parsePL
                symbol ")"
                return (Or wf1 wf2)

parseAnd :: Parser Prop
parseAnd   = do symbol "("
                wf1 <- parsePL
                symbol "/\\"
                wf2 <- parsePL
                symbol ")"
                return (And wf1 wf2)

parseIff :: Parser Prop
parseIff   = do symbol "("
                wf1 <- parsePL
                symbol "<=>"
                wf2 <- parsePL
                symbol ")"
                return (Iff wf1 wf2)

parsePL :: Parser Prop
parsePL = parseAtom +++ parseImply +++ parseNot +++
          parseOr   +++ parseAnd +++   parseIff

deP :: Parser Prop -> String -> [(Prop, String)]
deP (P x) = x

parse :: String -> Maybe Prop
parse input = case (deP parsePL input) of
                   ((wf,rest):_) -> Just wf
                   []            -> Nothing
