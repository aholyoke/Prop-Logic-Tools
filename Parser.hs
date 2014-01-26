{-# LANGUAGE DeriveDataTypeable #-}
module Parser (pFormula, p, parseTree, ParseTree(Variable, Negation, Biconditional, Conditional, Conjunction, Disjunction)) where
import Text.ParserCombinators.Parsec ((<|>), Parser, char, letter, parse, try)
import Text.Parsec.Expr
import Text.Parsec.Error (ParseError)
import Text.Parsec.Char (string)
import Text.Parsec.Token (reservedOp, makeTokenParser, reservedOpNames, parens)
import Text.Parsec.Language (emptyDef)
import Data.Data (Data, Typeable)

data ParseTree =
	  Conjunction ParseTree ParseTree
	| Disjunction ParseTree ParseTree
	| Conditional ParseTree ParseTree
	| Biconditional ParseTree ParseTree
	| Negation ParseTree
	| Variable Char
	deriving (Eq, Data, Typeable) -- Data and Typeable allow uniplate library to be used in Algorithms.hs

-- Topmost parser combinator
pFormula :: Parser ParseTree
pFormula = buildExpressionParser grammar term

-- Order of precedence is top to bottom (eg. negation binds tightest)
grammar = [ [Prefix negation]
	, [Infix conjunction AssocLeft]
	, [Infix disjunction AssocLeft]
	, [Infix conditional AssocLeft]
	, [Infix biconditional AssocLeft]
	]

parenthesis :: Parser ParseTree
parenthesis = do {char '('; x <- pFormula; char ')'; return x}

--Primitive parsers
--term, negation, conjunction, disjunction, conditional, biconditional, variable :: Parser ParseTree
term = variable <|> parenthesis <|> pFormula
negation = string "~" >> return (Negation)
conjunction = string "^" >> return (Conjunction)
disjunction = string "|" >> return (Disjunction)
conditional = string "->" >> return (Conditional)
biconditional = string "<->" >> return (Biconditional)
variable = letter >>= \name -> return $ Variable name


-- Useful parse function for testing purposes (Assumes there is no parsing error)
p s = case parse pFormula "" s of ~(Right tree) -> tree

-- Actually parse the string
parseTree :: String -> Either ParseError ParseTree
parseTree = parse pFormula ""
