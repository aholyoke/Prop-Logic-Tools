module Interpreter where
import Parser
import Text.Parsec hiding (Line)
import Text.ParserCombinators.Parsec (Parser)
import qualified Data.Map as Map
import PrettyPrinter
import Algorithms

data Line = Assignment {name :: String, formula :: ParseTree}
	  | Command    {function :: ParseTree -> String, formula :: ParseTree}

instance Show Line where
	show (Assignment name formula) = name ++ "= " ++ show formula
	show (Command function formula) = show formula ++ " becomes " ++ function formula

{-either new assignment or a command-}
runLine :: Line -> Either String (String, ParseTree)
runLine (Command function formula) = Left (function formula)
runLine (Assignment name formula) = Right (name, formula)

{-right is a string formula attached to it's parse tree,
  left is a string to output
-}
process :: String -> Either String (String, ParseTree)
process input
	|input == "q" = Left "Exiting"
	|otherwise = (runLine . parseLine) input

interpreterLoop :: Map.Map String ParseTree -> IO ()
interpreterLoop heap = do
	putStr "::> "
	input <- getLine
	case process input of
		(Left s) -> putStrLn s
		(Right (name, form)) -> interpreterLoop (Map.insert name form heap)

commandNames :: [(String, ParseTree -> String)]
commandNames = [ ("TruthTable", format . answer)
		,("ConjunctiveNormalForm", show . conjunctiveNormalForm)
		,("CNF", show . conjunctiveNormalForm)
		,("NegationNormalForm", show . negationNormalForm) 
		,("NNF", show . negationNormalForm) ]

parseLine :: String -> Line
parseLine str = case parse pLine "" str of
	(Left err) -> undefined -- Unable to parse
	(Right line) -> line

pLine :: Parser Line
pLine = try pAssignment <|> pCommand

pAssignment :: Parser Line
pAssignment = do
	name <- many1 letter
	optional space
	char '='
	optional space
	formula <- pFormula
	return (Assignment name formula)

pCommand :: Parser Line
pCommand = do
	function <- many1 letter
	let func =  case lookup function commandNames of
			(Just correct) -> correct
			Nothing -> undefined -- User mistyped function name
	space
	formula <- pFormula
	return (Command func formula)

