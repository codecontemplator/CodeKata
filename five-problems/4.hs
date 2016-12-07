-- http://www.shiftedup.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour

import Data.Char
import Data.List

compareX :: Int -> Int -> Ordering
compareX a b = compareX' (show a) (show b)
    where
    compareX' :: String -> String -> Ordering
    compareX' [] [] = EQ
    compareX' [] _ = LT
    compareX' _ [] = GT
    compareX' (x:xs) (y:ys) = 
        let
            x' = digitToInt x
            y' = digitToInt y
            r = compare x' y'
        in
            if r == EQ then
                compareX' xs ys
            else 
                r

makeLargestNum :: [Int] -> Int
makeLargestNum xs = read $ concat $ map show $ sortBy (flip compareX) xs 

