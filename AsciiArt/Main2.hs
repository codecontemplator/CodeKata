import Text.Regex
import Data.Maybe

textSample =
  "--  \n"
  ++"-- Here's a thing\n"
  ++"-- Here's another thing\n"
  ++"-- The most important thing\n"
  ++"-- *    *     *\n"
  ++"--\n"
  ++"f x=x+1*x*1*1*0\n"

data LineParser a = LineParser ([String] -> Maybe ([String],a))

instance Monad LineParser where
    return x = LineParser (\s -> Just (s,x))
    (LineParser p) >>= f =
        LineParser $ \s ->
            case p s of
                Nothing -> Nothing
                Just (s',x) -> let (LineParser p') = f x in p' s'

instance Functor LineParser where
    fmap f (LineParser p) = LineParser $ \s -> case p s of
                                          Nothing -> Nothing
                                          Just(s,x) -> Just (s, f x)

many :: LineParser a -> LineParser [a]
-- p :: LineParser ([String] -> Maybe ([String],a))
-- many :: LineParser ([String] -> Maybe ([String],[a]))
many (LineParser p) = LineParser $ applyPUntilFailure []
    where
      applyPUntilFailure xs rows =
        case p rows of
          Nothing -> Just (rows,xs)
          Just (rows',x) -> applyPUntilFailure (xs++[x]) rows'

many1 :: LineParser a -> LineParser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

data CodeLine =
    EmptyComment String
  | Label String
  | XMarker String
  | Code String deriving(Show)

mkLineParser :: (String -> Maybe String) -> LineParser String
mkLineParser f = LineParser $ \rows ->
  case rows of
    [] -> Nothing
    (x:xs) ->
      case f x of
        Nothing -> Nothing
        Just r -> Just (xs,r)

match :: String -> (String -> Maybe String)
match re = \s ->
  case matchRegex (mkRegex re) s of
    Just (x:_) -> Just x
    _ -> Nothing

emptyComment :: LineParser CodeLine
emptyComment = fmap EmptyComment $ mkLineParser $ match "^--([ \t\r\n\v\f]*)$"

label :: LineParser CodeLine
label = fmap Label $ mkLineParser $ match "^--[ \t\r\n\v\f]*(.*)$"

markerLine :: LineParser CodeLine
label = fmap Label $ mkLineParser $ match "^--([ \t\r\n\v\f\\*])*$"

(+++) :: LineParser [a] -> LineParser [a] -> LineParser [a]
(+++) pa pb = do
  xs <- pa
  ys <- pb
  return (xs ++ ys)

codeblock :: LineParser [CodeLine]
codeblock = emptyComments +++ labels +++ markerLines -- >> emptyComments >> codeLine
  where
    emptyComments = many emptyComment
    labels = many1 label
    markerLines = many1 markerLine

parse :: LineParser a -> String -> Maybe a
parse (LineParser p) text =
  case p (lines text) of
    Just (_, result) -> Just result
    Nothing -> Nothing

--test text = parse block codeBlock
