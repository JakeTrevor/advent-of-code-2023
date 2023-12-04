module Day1
  ( part1,
    part2,
  )
where

import Data.Char (isDigit)
import Lib (Solution)
import Text.Parsec

extractNumber :: String -> String
extractNumber [] = ""
extractNumber (c : str)
  | isDigit c = c : extractNumber str
  | otherwise = extractNumber str

getValue :: (String -> String) -> String -> Int
getValue _ [] = error "each line must have at least one number"
getValue f s = read [head numChars, last numChars]
  where
    numChars = f s

getValues :: (String -> String) -> String -> [Int]
getValues f input = map (getValue f) $ lines input

part1 :: Solution
part1 t = do
  content <- readFile t
  let values = getValues extractNumber content
  print $ sum values

-- * Part 2:

number :: Parsec String () Char
number =
  choice
    [ string "one" >> return '1',
      try (string "two" >> return '2'),
      string "three" >> return '3',
      try (string "four" >> return '4'),
      string "five" >> return '5',
      try (string "six" >> return '6'),
      string "seven" >> return '7',
      string "eight" >> return '8',
      string "nine" >> return '9'
    ]

numberOrDigit :: Parsec String () Char
numberOrDigit = number <|> digit

parseCode :: String -> String
parseCode [] = []
parseCode s =
  let result = parse numberOrDigit "" s
   in case result of
        Right val -> val : parseCode (tail s)
        Left _ -> parseCode (tail s)

part2 :: Solution
part2 t = do
  content <- readFile t
  let values = getValues parseCode content
  print $ sum values
