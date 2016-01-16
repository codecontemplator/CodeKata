-- http://codegolf.stackexchange.com/questions/69442/make-a-simple-pretty-comment-tool
-- package: https://hackage.haskell.org/package/regex-applicative-0.2.1/docs/Text-Regex-Applicative.html
-- example: https://github.com/feuerbach/regex-applicative/wiki/Examples
-- cabal install regex-applicative

-- ghci overloadedStrings -> :set -XOverloadedStrings


--{-# LANGUAGE OverloadedStrings #-}

import Text.Regex.Applicative
import Data.Char hiding (Space)
import Data.Maybe

data CodeLine =
    EmptyComment
  | Label String
  | Marker [String]
  | Code String deriving(Show)

isEOL :: Char -> Bool
isEOL = ('\n'==)

anyChar :: RE Char Char
anyChar = psym (not.isEOL)

space :: RE Char String
space = many $ psym isSpace

linify :: RE Char CodeLine -> RE Char CodeLine
linify re = string "--" *> re <* (psym isEOL)

emptyComment :: RE Char CodeLine
emptyComment = linify $ EmptyComment <$ space

marker :: RE Char CodeLine
marker = linify $ Marker <$> many (space <|> string "*")

label :: RE Char CodeLine
label = linify $ space *> (Label <$> many anyChar)

code :: RE Char CodeLine
code = (Code <$> many anyChar) <* (psym isEOL)

block :: RE Char [CodeLine]
block = many (emptyComment <|> marker <|> label <|> code)

textSample :: String
textSample =
    "--  \n"
  ++"-- Here's a thing\n"
  ++"-- Here's another thing\n"
  ++"-- The most important thing\n"
  ++"-- *    *     *\n"
  ++"--\n"
  ++"f x=x+1*x*1*1*0\n"

transform text =
  case text =~ block of
    Nothing -> Nothing
    Just parts ->
      let
        isLabel :: CodeLine -> Bool
        isLabel x = case x of Label _ -> True; _ -> False
        isMarker :: CodeLine -> Bool
        isMarker x = case x of Marker _ -> True; _ -> False
        isCode :: CodeLine -> Bool
        isCode x = case x of Code _ -> True; _ -> False
        getLabel :: CodeLine -> String
        getLabel x = case x of Label s -> s; _ -> ""
        getCode :: CodeLine -> String
        getCode x = case x of Code s -> s; _ -> ""
        labels :: [String]
        labels = map getLabel $ filter isLabel parts
        markerDef :: [String]
        markerDef = case head (filter isMarker parts) of
                    Marker xs -> xs
                    _ -> []
        code :: [String]
        code = map getCode $ filter isCode parts
        display :: String
        display = display' labels markerDef ([],"--","--")
        minusSigns :: Int -> String
        minusSigns num = take num $ repeat '-'
        rowWidth :: Int
        rowWidth = sum $ map length markerDef
        display' :: [String] -> [String] -> ([String],String,String) -> String
        display' [] _ (rows,arrowLine,rowTemplate) =
            unlines $ rows ++ [rowTemplate,arrowLine] ++ code
        display' (l:ls) (m:ms) (rows,arrowLine,rowTemplate) =
          if all isSpace m then
            display' (l:ls) ms (rows,arrowLine++m,rowTemplate++m)
          else
            let
              arrowLine' = arrowLine ++ "v"
              rowTemplate' = rowTemplate ++ "|"
              row :: String
              row = rowTemplate ++ "/" ++ (minusSigns (rowWidth+2-length rowTemplate)) ++ "< " ++ l
            in
              display' ls ms (rows++[row],arrowLine',rowTemplate')
      in
        Just display

test = writeFile "C:/temp/test.txt" (fromJust (transform textSample))
