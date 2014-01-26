module Algorithms where
import Data.List (nub, transpose)
import Data.Generics.Uniplate.Data --Used extensively to scrap recursive boilerplate
import Control.Monad  (replicateM)
import Parser

type Table = [(ParseTree, [Bool])]

-- setup all valuations and evaluate given formula under each valuation
answer :: ParseTree -> Table
answer tree = setup ++ [(tree, map (evaluate tree) rows)]
	where setup = (allValuations . getVars) tree :: Table
	      rows = map r [0..2 ^ length setup - 1] :: [[(ParseTree, Bool)]]
	      r n = map (\(v, bools) -> (v, bools !! n)) setup

-- Collect all Variables in tree
getVars :: ParseTree -> [ParseTree]
getVars tree = nub [result | result@(Variable a) <- universe tree]


-- Build first half of Table where variables are assigned to all combinations of T/F
allValuations :: [ParseTree] -> Table 
allValuations vars = zip vars permutations
	where permutations = transpose $ replicateM (length vars) [True, False]


-- Evaluate parsed formula under given valuation (ie. Evaluate single row)
evaluate :: ParseTree -> [(ParseTree, Bool)] -> Bool
evaluate (Conjunction   a b) vars = evaluate a vars && evaluate b vars
evaluate (Disjunction   a b) vars = evaluate a vars || evaluate b vars
evaluate (Conditional   a b) vars = not (evaluate a vars) || evaluate b vars
evaluate (Biconditional a b) vars = evaluate a vars == evaluate b vars
evaluate (Negation      a  ) vars = not $ evaluate a vars
evaluate v ((b, bool):xs)  --Find assignments of v
	| v == b = bool
	| otherwise = evaluate v xs

{-Transformations-}
-- Reduce formula to adequate set {Conjunction, Negation}. Double negatives are unacceptable
reduceConnectives :: ParseTree -> ParseTree
reduceConnectives = removeDoubleNegatives . (transform (\tree -> case tree of
	(phi `Disjunction` psi) -> Negation $ (Negation $ reduceConnectives phi) `Conjunction` (Negation $ reduceConnectives psi)
	otherwise -> otherwise)) . reduceConditional . reduceBiconditional

-- Replace all consecutive Negations with their scope
removeDoubleNegatives :: ParseTree -> ParseTree
removeDoubleNegatives = transform (\tree -> case tree of
    Negation (Negation phi) -> phi
    otherwise -> otherwise)

-- Create equivalent formula in CNF. CNF is a conjunction of clauses, a clause is a disjunction of literals.
-- Examples:
--  r->(s->(t^s->r)) -> ~r|~s|~t|~s|r            --Single clause 
--  a<->(b^c)        -> (~a|b)^(~a|c)^(~b|~c|~a) --Disjunction of clauses
--  a^(b->c)         -> a^(~b|c)                 --Clause with single literal
conjunctiveNormalForm :: ParseTree -> ParseTree
conjunctiveNormalForm = (transform (\tree -> case tree of
		(phi `Disjunction` psi) -> distr (conjunctiveNormalForm phi) (conjunctiveNormalForm psi)
		otherwise -> otherwise)) . negationNormalForm

-- Use distributive property of Conjunction/Disjunction 
-- Example:
-- (a^b)|c -> (a|c)^(b|c)  
-- Precondition: distr a b where a and b are in CNF 
-- Precondition: distr a b where a and b are seperated by disjunction 
distr (n11 `Conjunction` n12) n2 = (distr n11 n2) `Conjunction` (distr n12 n2)
distr n1 (n21 `Conjunction` n22) = (distr n1 n21) `Conjunction` (distr n1 n22)
distr n1 n2 = n1 `Disjunction` n2

-- Create equivalent formula in NNF. 
-- Remove Biconditionals and Conditionals, then use deMorgan's to expand all negations into their scopes, finally remove any leftover double negatives
-- We are left with only Conjunctions, Disjunctions and Literals
negationNormalForm :: ParseTree -> ParseTree
negationNormalForm = removeDoubleNegatives . deMorgansExpansion . reduceConditional . reduceBiconditional

deMorgansExpansion :: ParseTree -> ParseTree
deMorgansExpansion = transform (\tree -> case tree of
			Negation (phi `Conjunction` psi) -> (negationNormalForm $ Negation phi) `Disjunction` (negationNormalForm $ Negation psi)
			Negation (phi `Disjunction` psi) -> (negationNormalForm $ Negation phi) `Conjunction` (negationNormalForm $ Negation psi)
			otherwise -> otherwise)

-- Replace all biconditionals a<->b with (a->b)^(b->a)
reduceBiconditional :: ParseTree -> ParseTree
reduceBiconditional = transform (\tree -> case tree of
	(phi `Biconditional` psi) -> (reduceBiconditional phi `Conditional`  reduceBiconditional psi) `Conjunction` (reduceBiconditional psi `Conditional` reduceBiconditional phi)
	otherwise -> otherwise)

-- Replace all conditionals a->b with ~a|b 
reduceConditional :: ParseTree -> ParseTree
reduceConditional = transform (\tree -> case tree of
	(phi `Conditional` psi) -> (Negation $ reduceConditional phi) `Disjunction` (reduceConditional psi)
	otherwise -> otherwise)
