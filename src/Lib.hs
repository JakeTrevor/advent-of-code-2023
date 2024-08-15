module Lib
  ( Solution,
    parseNumber,
    parse,
  )
where

import Text.Parsec (ParseError, Parsec, runParser)

type Solution = String -> IO ()

parse :: String -> Parsec String () a -> a
parse text parser = errorIfEvil $ runParser parser () "" text

errorIfEvil :: Either ParseError a -> a
errorIfEvil (Right g) = g
errorIfEvil (Left e) = error $ show e

parseNumber :: Parsec String () Int
parseNumber = read <$> (many1 digit <* spaces)
