-- http://www.shiftedup.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour

import Data.Char
import Data.List

data Op = Minus | Plus
data E = T Int | E Int Op E

instance Show Op where
    show Minus = "-"
    show Plus = "+"

instance Show E where
    show (T i) = show i
    show (E i o e) = show i ++ show o ++ show e

eval e = eval' Plus e
    where 
        eval' Plus (T i) = i
        eval' Minus (T i) = -i
        eval' Plus (E i op e) = i + eval' op e
        eval' Minus (E i op e) = -i + eval' op e

(<+>) :: Int -> E -> E
(<+>) x e = E x Plus e

(<->) :: Int -> E -> E
(<->) x e = E x Minus e

(<|>) :: Int -> E -> E
(<|>) x1 (T x2) = T (read (show x1 ++ show x2))
(<|>) x1 (E x2 o e) = E (read (show x1 ++ show x2)) o e

generate :: [Int] -> [E]
generate [x] = [T x]
generate (x:xs) = concatMap (\y -> [x <+> y, x <-> y, x <|> y]) (generate xs)

solve = filter (\e -> eval e == 100) (generate [1..9])
