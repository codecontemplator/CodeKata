-- http://www.shiftedup.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour

import Data.List

compareX :: Int -> Int -> Ordering
compareX a b =compare (a' ++ b') (b' ++ a')
    where 
        a' = show a
        b' = show b

makeLargestNum :: [Int] -> Int
makeLargestNum xs = read $ concat $ map show $ sortBy (flip compareX) xs 
