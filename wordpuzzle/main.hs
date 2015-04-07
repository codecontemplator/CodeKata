-- http://codekata.com/kata/kata19-word-chains/
-- dictionary from: http://www.mieliestronk.com/corncob_lowercase.txt

import Data.List --hiding (union,insert,delete)
import Data.Maybe
import Debug.Trace
import Data.Ord
import Data.Graph
import Data.Array
import Data.Tree
--import Data.Graph.Inductive.Graph

type Dictionary = [String]

traceex x = trace (show x) $ x

distance :: Eq a => [a] -> [a] -> Int
distance [] [] = 0
distance [] _  = maxBound
distance _ []  = maxBound
distance (x:xs) (y:ys) = (if x == y then 0 else 1) + distance xs ys 

edgeList :: [String] -> [(String, String,[String])]
edgeList dictionary = map edge dictionary
	where edge n = (n, n, filter (\n' -> distance n n' == 1) dictionary) 

graph :: [String] -> (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
graph dictionary = graphFromEdges $ edgeList dictionary

-- ref: http://stackoverflow.com/questions/21400111/how-to-find-path-to-a-node-in-a-haskell-data-tree
pathsToNode :: Eq a => a -> Tree a -> [[a]]
pathsToNode x (Node y ns) = [[x] | x == y] ++ map (y:) (pathsToNode x =<< ns)

search :: String -> String -> Dictionary -> [[String]]-- [[String]]
search source target dictionary =
	let
		(g,v2w,w2v) = graph dictionary 
		source' = w2v' source
		target' = w2v' target
		v2w' :: Vertex -> String
		v2w' v = case v2w v of (x,_,_) -> x
		w2v' w = fromJust $ w2v w
		trees = dfs g [source']
		tree = head $ trees
		paths :: [[Vertex]]
		paths = pathsToNode target' tree
	in
		trace (show (length trees)) $
		--Data.Tree.drawTree (traceex (fmap v2w' tree))
		map (\l->map v2w' l) paths

{-
search :: String -> String -> Dictionary -> [[String]]
search source target dictionary =
	let
		(g,v2w,w2v) = graph dictionary 
		v2w' v = case v2w v of (x,_,_) -> x
		w2v' w = fromJust $ w2v w
		source' = w2v' source
		target' = w2v' target
		search' s t path  =
			let 
				neighbours        = g!s
				path'             = s : path
				search'' s'       = search' s' t path'
			in
				--aweertrace ((v2w' s) ++ ":" ++ (show (map v2w' neighbours))) $ 
				if s == t then
					[map v2w' path']
				else
					concatMap search'' (sortBy (comparing(\v -> distance (v2w' v) target)) (neighbours \\ path))

	in
		reachable g source'

		--map reverse $ search' source' target' []

-}

{-
search :: String -> String -> Dictionary -> [[String]]
search source target dictionary =
	let 
		dictionary' = filter (\s -> length source == length s) dictionary
		search' s t path  =
			let 
				neighbours        = filter (\s' -> distance s s' == 1) dictionary'				
				path'             = s : path
				search'' s'       = search' s' t path'
			in
				if s == t then
					[reverse path']
				else
					concatMap search'' (sortBy (comparing (\s' -> distance s' t)) (neighbours \\ path))
	in
		search' source target []
-}

main = do
	contents <- readFile "dictionary.txt"
---	putStrLn  (show(search "cat" "dog" (lines contents)))
	let s = search "cat" "dog" (lines contents)
	putStrLn $ show $ head $ s 

{-
puzzle :: String -> String -> [String] -> [String]
puzzle source target dictionary =
    let 
        dictionary' :: Set String
        dictionary' = fromList $ filter (\s -> length s == length source) dictionary 
        puzzle' :: String -> String -> [String] -> [String]
        puzzle' s t chain  =
        	trace (s ++ ";" ++ t ++ ";" ++ (show chain)) $
            if s == t then 
                chain ++ [t]
            else
            	filter (distance1 s && not(member chain)) dictionary'
                case find (\w -> member w dictionary') (traceex (candidates s t)) of
                    Nothing -> []
                    Just w -> puzzle' w t (chain ++ [s]) 
    in
        puzzle' source target []


main = do
    putStrLn "Source (cat?) : "
    source <- getLine
    putStrLn "Target (dog?) : "
    target <- getLine
    contents <- readFile "dictionary.txt"
    let dictionary = lines contents in
	    let chain = puzzle source target dictionary in
			putStrLn $ source ++ " -> " ++ target ++ " : " ++ (show chain)

-}
 