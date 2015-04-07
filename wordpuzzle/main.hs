-- http://codekata.com/kata/kata19-word-chains/
-- dictionary from: http://www.mieliestronk.com/corncob_lowercase.txt

import Data.List --hiding (union,insert,delete)
import Data.Maybe
import Debug.Trace
import Data.Ord
--import Data.Graph
import Data.Array
import Data.Tree
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.SP

type Dictionary = [String]

traceex x = trace (show x) $ x

distance :: Eq a => [a] -> [a] -> Int
distance [] [] = 0
distance [] _  = 10000
distance _ []  = 10000
distance (x:xs) (y:ys) = (if x == y then 0 else 1) + distance xs ys 

graph :: [String] -> Gr String Int
graph dictionary = 
	let
		nodes :: [LNode String]
		nodes = zip [0..] dictionary
		edges :: [LEdge Int]
		edges = concatMap neighbours nodes
		neighbours :: LNode String -> [LEdge Int]	
		neighbours n@(i,w) = map (\(i',_) -> (i,i',1)) (filter (\(_,w') -> distance w w' == 1) nodes)
	in
		mkGraph nodes edges


addIfNotPresent :: [String] -> [String] -> [String]
addIfNotPresent dictionary words = dictionary ++ newWords
	where newWords = filter (\w -> notElem w dictionary) words

main = do
	contents <- readFile "dictionary.txt"
	let sourceString = "ruby"
	let destString = "code"
	let dictionary_ = filter (\w -> length w == length sourceString) (lines contents)
	let dictionary = addIfNotPresent dictionary_ [sourceString, destString]
	let g = graph dictionary	
	let toIndex w = fromJust $ elemIndex w dictionary
	let fromIndex i = dictionary!!i
	let source = toIndex sourceString
	let target = toIndex destString
	let path = sp source target g
	putStrLn $ show $ map fromIndex path
