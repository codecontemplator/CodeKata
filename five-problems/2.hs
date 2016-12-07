-- http://www.shiftedup.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour

combineAlternating _ [] = []
combineAlternating [] _ = []
combineAlternating (x:xs) (y:ys) = x:y:combineAlternating xs ys

