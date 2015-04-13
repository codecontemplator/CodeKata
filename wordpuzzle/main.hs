{-# LANGUAGE BangPatterns #-}
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
import Data.Graph.Inductive.Tree
--import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.SP
--import Control.Monad
--import Control.Monad.Trans
--import Control.Monad.LoopWhile
--import Data.Function.Memoize

type Dictionary = [String]

traceex x = trace (show x) $ x

distance :: [Char] -> [Char] -> Int
--distance [] [] = 0
--distance [] _  = 10000
--distance _ []  = 10000
--distance (x:xs) (y:ys) = (if x == y then 0 else 1) + distance xs ys 
distance xs ys = 
		if length xs == length ys then
			distance' 0 xs ys
		else
			max
	where
		max = 10000
		distance' d [] [] = d
		distance' d (x:xs) (y:ys) =
			case (x==y, d) of
				(False, 0)  -> distance' 1 xs ys
				(False, _)  -> max
				(True, d) -> distance' d xs ys


graph :: [String] -> Gr String Int
graph dictionary = 
    let
        nodes :: [LNode String]
        !nodes = zip [0..] dictionary
        edges :: [LEdge Int]
        !edges = concatMap neighbours nodes
        neighbours :: LNode String -> [LEdge Int]    
        neighbours n@(i,w) = map (\(i',_) -> (i,i',1)) (filter (\(_,w') -> distance w w' == 1) nodes)
    in
        mkGraph (trace "nodes" nodes) (trace "edges" edges)


addIfNotPresent :: [String] -> [String] -> [String]
addIfNotPresent dictionary words = dictionary ++ newWords
    where newWords = filter (\w -> notElem w dictionary) words

main = do
    contents <- readFile "dictionary.txt"
    let dictionary_ = lines contents
    let dictionary = filter (\s -> length s < 5) $ addIfNotPresent dictionary_ ["ruby"]
    let !g = graph dictionary    
    let toIndex w = fromJust $ elemIndex w dictionary
    let fromIndex i = dictionary!!i
    let loop = do
    	putStrLn "source:"
        sourceString <- getLine
        putStrLn "target:"
        destString <- getLine
        let source = toIndex sourceString
        let target = toIndex destString
        let path = sp source target g
        putStrLn $ show $ map fromIndex path
        loop
    loop