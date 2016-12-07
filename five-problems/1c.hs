-- http://www.shiftedup.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour

sumUsingRecursion [] = 0
sumUsingRecursion (x:xs) = x + sumUsingRecursion xs

