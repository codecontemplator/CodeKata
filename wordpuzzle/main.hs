-- http://codekata.com/kata/kata19-word-chains/
-- dictionary from: http://www.mieliestronk.com/corncob_lowercase.txt

import Data.List --hiding (union,insert,delete)
import Data.Maybe
import Debug.Trace
import Data.Ord

type Dictionary = [String]

traceex x = trace (show x) $ x

distance :: Eq a => [a] -> [a] -> Int
distance [] [] = 0
distance [] _  = maxBound
distance _ []  = maxBound
distance (x:xs) (y:ys) = (if x == y then 0 else 1) + distance xs ys 

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

main = do
	contents <- readFile "dictionary.txt"
	putStrLn  (show(search "cat" "dog" (lines contents)))

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
 