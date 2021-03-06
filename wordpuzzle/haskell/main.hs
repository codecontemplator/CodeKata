{-# LANGUAGE BangPatterns #-}
-- http://codekata.com/kata/kata19-word-chains/
-- dictionary from: http://www.mieliestronk.com/corncob_lowercase.txt

import Data.List
import Data.Maybe
import Debug.Trace
import Data.Ord
import Data.Array
import Data.Tree
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Query.SP
import Data.Time.Clock

type Word = String
type Dictionary = [Word]

neighbours :: Word -> Word -> Bool
neighbours xs ys = 
        length xs == length ys &&  distance 0 xs ys == 1
	where
		distance d [] [] = d
		distance d (x:xs) (y:ys) =
			case (x==y, d) of
				(False, 0)  -> distance 1 xs ys
				(False, _)  -> -1 -- no use to continue... 
				(True, d) -> distance d xs ys

graph :: Dictionary -> Gr Word Int
graph dictionary = 
    let
        nodes :: [LNode Word]
        nodes = zip [0..] dictionary
        edges :: [LEdge Int]
        edges = [(i,i',1) | (i,w) <- nodes, (i',w') <- nodes, i /= i', neighbours w w']
    in
        mkGraph nodes edges

main = do
    contents <- readFile "dictionary.txt"
    let dictionary = lines contents
    putStrLn "Initializing..."
    t1 <- getCurrentTime
    let !g = graph dictionary
    t2 <- getCurrentTime
    putStrLn $ "Done. Took " ++ (show (diffUTCTime t2 t1))
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
