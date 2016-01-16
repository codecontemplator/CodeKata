{-# LANGUAGE BangPatterns #-}
-- http://codekata.com/kata/kata19-word-chains/
-- http://www.wordchains.com/
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
data Ruleset = Codekata | Wordchains

anagram :: Word -> Word -> Bool
anagram xs ys = xs /= ys && sort xs == sort ys

chardiff :: Word -> Word -> Int
chardiff xs ys = length $ filter (\(a,b) -> a /= b) $ zip xs ys

substrings :: Word -> [Word]
substrings xs = unfoldr generate ([],xs)
    where
        generate (_,[])      = Nothing
        generate (xs,(y:ys)) = Just (xs++ys, (xs++[y],ys))

neighbours_codekata :: Word -> Word -> Bool
neighbours_codekata xs ys = 
        case length xs - length ys of
            0  -> chardiff xs ys == 1
            _  -> False

neighbours_wordchains :: Word -> Word -> Bool
neighbours_wordchains xs ys = 
        case length xs - length ys of
            0  -> chardiff xs ys == 1 || anagram xs ys
            1  -> elem ys (substrings xs)
            -1 -> elem xs (substrings ys)
            _  -> False

graph :: Dictionary -> Ruleset -> Gr Word Int
graph dictionary ruleset = 
    let
        neighbours = case ruleset of Codekata -> neighbours_codekata; Wordchains -> neighbours_wordchains;
        nodes :: [LNode Word]
        nodes = zip [0..] dictionary
        edges :: [LEdge Int]
        edges = [(i,i',1) | (i,w) <- nodes, (i',w') <- nodes, i /= i', neighbours w w']
    in
        mkGraph nodes edges

main = do
    putStrLn "Select ruleset: 1=codekata.com, 2=wordchains.com"
    ruleset <- fmap (\x -> if x == "1" then Codekata else Wordchains) getLine
    contents <- readFile "dictionary.txt"
    let dictionary = lines contents
    putStrLn "Initializing..."
    t1 <- getCurrentTime
    let !g = graph dictionary ruleset
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
