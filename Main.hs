module Main (main, Table) where
import System.Environment (getArgs)
import Parser
import PrettyPrinter
import Algorithms
import Interpreter (interpreterLoop)
import Data.Map (empty)

makeTable :: String -> IO ()
makeTable formula = case parseTree formula of
	(Left error) -> print error
	(Right tree) -> pp $ answer tree


equivalent tree1 tree2 = (snd . last . answer) tree1  == (snd . last . answer) tree2
tautEquivalent :: String -> String -> IO () 
tautEquivalent a b = case parseTree a of
	(Left error) -> print $ "First formula error: " ++ show error
	(Right tree1) -> case parseTree b of
		(Left error) -> print $ "Second formula error: " ++ show error
		(Right tree2) -> print $ equivalent tree1 tree2

main :: IO()
main = do
	readFile "OpeningMessage.txt" >>= putStrLn
	interpreterLoop empty
