module Day5
  ( part1,
    part2,
  )
where

import Data.Char (isDigit)
import Lib (Solution, parse, parseNumber)
import Text.Parsec (Parsec, many1, oneOf)
import Text.Parsec.Char (spaces)

validCards = "AKQJT98765432"

data Card = Ace | King | Queen | Jack | Number {val :: Int}

type Hand = [Card]

data Player = MkPlayer {hand :: Hand, bid :: Int}

data Score = FiveOK | FourOK | FullHouse | ThreeOK | TwoPair | OnePair | HighCard
  deriving (Eq, Ord)

getCard :: Char -> Card
getCard 'A' = Ace
getCard 'K' = King
getCard 'Q' = Queen
getCard 'J' = Jack
getCard s
  | isDigit s = Number $ read [s]
  | otherwise = undefined

parseCard :: Parsec String () Card
parseCard = getCard <$> oneOf validCards

parseHand :: Parsec String () Hand
parseHand = many1 parseCard <* spaces

parsePlayer :: Parsec String () Player
parsePlayer = do
  hnd <- parseHand
  bet <- parseNumber
  return MkPlayer {hand = hnd, bid = bet}

parsePlayers :: Parsec String () [Player]
parsePlayers = many1 parsePlayer

getScore :: Hand -> Int
getScore [a, b, c, d, e] = undefined
getScore _ = undefined

part1 :: Solution
part1 t = do
  content <- readFile t
  let players = parse content parsePlayers

  return ()

part2 :: Solution
part2 t = do
  content <- readFile t
  return ()
