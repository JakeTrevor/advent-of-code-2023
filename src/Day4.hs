module Day4
  ( part1,
    part2,
  )
where

import Lib (Solution)
import Text.Parsec (Parsec, char, digit, many1, optional, parse, spaces, string)

data ScratchCard = MkCard {cardNumber :: Int, winningNumbers :: [Int], presentNumbers :: [Int]}

cardName :: Parsec String () Int
cardName =
  do
    _ <- string "Card"
    spaces
    d <- many1 digit
    _ <- char ':'
    spaces
    return $ read d

number :: Parsec String () Int
number = do
  d <- many1 digit
  spaces
  return $ read d

card :: Parsec String () ScratchCard
card = do
  n <- cardName
  winners <- many1 number
  _ <- char '|'
  spaces
  present <- many1 number
  return $ MkCard n winners present

cards :: Parsec String () [ScratchCard]
cards = many1 (card <* optional (char '\n'))
-- ^ ^ equiv?:

-- $ do
--   c <- card
--   optional $ char '\n'
--   return c

parseFile :: String -> [ScratchCard]
parseFile s = case parse cards "" s of
  Right val -> val
  Left e -> error $ show e

getNumMatches :: ScratchCard -> Int
getNumMatches MkCard {winningNumbers = winners, presentNumbers = nums} =
  length (filter (`elem` winners) nums) - 1

getScore :: ScratchCard -> Int
getScore c
  | k == (-1) = 0
  | otherwise = 2 ^ k
  where
    k = getNumMatches c - 1

part1 :: Solution
part1 t = do
  content <- readFile t
  let allCards = parseFile content
  let allValues = map getScore allCards
  print $ sum allValues

-- * part 2

updateCount :: [Int] -> [ScratchCard] -> [Int]
updateCount [] [] = []
updateCount (n : ns) (c : cs) = n : updateCount rest cs
  where
    matches = getNumMatches c
    rest = addToCounts ns matches n
updateCount _ _ = error "update count arrs must have matching lengths"

addToCounts :: [Int] -> Int -> Int -> [Int]
addToCounts ns matches change = foldl f ns a
  where
    a = [0 .. matches]
    f ns' i = replace ns' i change

replace :: [Int] -> Int -> Int -> [Int]
replace (x : xs) 0 a = (x + a) : xs
replace (x : xs) i a = x : replace xs (i - 1) a
replace _ _ _ = error "unreachable"

part2 :: Solution
part2 t = do
  content <- readFile t
  let allCards = parseFile content
  let initialCount = replicate (length allCards) 1
  let finalCount = updateCount initialCount allCards
  print $ sum finalCount
