-- http://codegolf.stackexchange.com/questions/69442/make-a-simple-pretty-comment-tool

import Data.Char(isDigit,isAlpha,isSpace)
import Control.Monad
import Test.HUnit

--------------------------------------------------------
-- Parser type
--------------------------------------------------------

data Parser a = Parser (String -> Maybe (String,a))

instance Monad Parser where
    return x = Parser (\s -> Just (s,x))
    (Parser p) >>= f =
        Parser $ \s ->
            case p s of
                Nothing -> Nothing
                Just (s',x) -> let (Parser p') = f x in p' s'

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of
                                          Nothing -> Nothing
                                          Just(s,x) -> Just (s, f x)

instance MonadPlus Parser where
    mzero = Parser (\s -> Nothing)
    (Parser pa) `mplus` (Parser pb) = Parser $ \s ->
        case pa s of
            Nothing -> pb s
            x -> x

--------------------------------------------------------
-- Parser combinators
--------------------------------------------------------
match :: (Char -> Bool) -> Parser Char
match f = Parser $ \s ->
    case s of
      (x:xs) -> if f x then Just (xs, x) else Nothing
      _ -> Nothing

char :: Char -> Parser Char
char c = match (c==)

many :: Parser a -> Parser [a]
many (Parser p) = fmap reverse $ Parser $ \s -> scan s []
    where
        scan s xs = case p s of
                      Nothing -> Just (s, xs)
                      Just(s',x) -> scan s' (x:xs)

option :: a -> Parser a -> Parser a
option dx (Parser p) = Parser $ \s ->
    case p s of
        Just (s',x) -> Just (s',x)
        _ -> Just(s, dx)

conditional :: (a -> Bool) -> Parser a -> Parser a
conditional c (Parser p) = Parser $ \s ->
    case p s of
        Just (s',x) -> if c x then Just (s',x) else Nothing
        _ -> Nothing

eol :: Parser ()
eol = do
  option ' ' (char '\r')
  char '\n'
  return ()

space :: Parser Char
space = match (\c -> c == ' ' || c == '\t')

spaces :: Parser [Char]
spaces = many space

row :: Parser [Char]
row = do
  char '-' >> char '-'
  spaces
  x <- many $ match ('\n'/=)
  spaces
  eol
  return x

rows :: (String -> Bool) -> Parser [String]
rows c = many $ conditional c row

comment = do
    rows isEmpty
    nonStarRows <- rows (not.isStar)
    stars <- conditional isStar row
    postEmpty <- rows isEmpty
    return (nonStarRows, stars, postEmpty)
  where
    isEmpty = all isSpace
    isStar s = all (\x -> isSpace x || x == '*') s && any (\x -> x == '*') s

--------------------------------------------------------
-- Parser helpers
--------------------------------------------------------

parse :: Parser a -> String -> Maybe a
parse (Parser p) s =
    case p s of
        Just (_,x) -> Just x
        _ -> Nothing

cline =
  "--\n"
  ++"-- Here's a thing\n"
  ++"-- Here's another thing\n"
  ++"-- The most important thing\n"
  ++"-- *    *     *\n"
  ++"--\n"
  ++"f x=x+1*x*1*1*0\n"
