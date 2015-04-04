-- http://codekata.com/kata/kata19-word-chains/
-- dictionary from: http://www.mieliestronk.com/corncob_lowercase.txt

import Data.Set hiding (filter)
import Data.List
import Debug.Trace

candidates :: String -> String -> [String]
candidates source target =
    let
        candidates' _ [] _ = []
        candidates' _ _ [] = []        
        candidates' zs (x:xs) (y:ys) = (zs ++ [y] ++ xs) : candidates' (zs ++ [x]) xs ys 
    in
        filter (/=source) (candidates' [] source target)

traceex x = trace (show x) $ x

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

 