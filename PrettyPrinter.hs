module PrettyPrinter  where
import Text.PrettyPrint.Boxes (text, hsep, render, left, vcat, (//))
import Data.List (transpose)
import Parser
import Algorithms

instance Show ParseTree where
	show (Variable a) = [a]
	show (Negation a) = '~' : display a
	show tree = (init . tail . display) tree
display (Conjunction a b)   = showParenthesis a "^" b
display (Disjunction a b)   = showParenthesis a "|" b
display (Conditional a b)   = showParenthesis a "->" b
display (Biconditional a b) = showParenthesis a "<->" b
display (Negation a)        = '~' : display a
display (Variable a)        = [a]
showParenthesis a op b      = concat ["(", display a, op, display b, ")"]


-- Prettyprint table
pp :: Table -> IO ()
pp = putStr . format

format :: Table -> String
format rep = render (header // table)
	where header = hsep 4 left $ map (\(ex, _) -> text $ show ex) rep
              table  = vcat left $ map (hsep 4 left) tableData
	      tableData = transpose $ map (\(_, bools) -> map (text . showBool) bools) rep
	      showBool True = "T"
	      showBool _    = "F"
