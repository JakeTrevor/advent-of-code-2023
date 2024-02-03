module Day6
  ( part1,
    part2,
  )
where

import Data.Maybe (catMaybes, mapMaybe)
import Lib (Solution, parse)
import Text.Parsec (Parsec, digit, many1, spaces, string)

data Race = MkRace {time :: Int, dist :: Int}

instance Show Race where
  show MkRace {time = t, dist = d} = "-x^2 + " ++ show t ++ "x + " ++ show d

parseNumber :: Parsec String () Int
parseNumber = read <$> (many1 digit <* spaces)

parseTimes :: Parsec String () [Int]
parseTimes = string "Time:" *> spaces *> many1 parseNumber

parseDistances :: Parsec String () [Int]
parseDistances = string "Distance:" *> spaces *> many1 parseNumber

parseFile :: Parsec String () [Race]
parseFile =
  do
    times <- parseTimes
    dists <- parseDistances
    return [MkRace {time = t, dist = d} | (t, d) <- zip times dists]

solve :: Race -> Maybe (Float, Float)
solve MkRace {time = b, dist = k} =
  -- names are using typical quadratic form
  let a = -1
      c = -k
      midPoint = (fromIntegral (-b) / fromIntegral (2 * a))
      delta = fromIntegral ((b ^ (2 :: Integer)) - (4 * a * c)) :: Float

      diff = sqrt delta / fromIntegral (2 * a)

      x1 = midPoint - diff
      x2 = midPoint + diff
   in if delta > 1 then Just (x1, x2) else Nothing

countWins :: (Float, Float) -> Int
countWins (x1, x2) = ceiling x1 - (floor x2 + 1)

part1 :: Solution
part1 t = do
  content <- readFile t
  let races = parse content parseFile
  let solutions = mapMaybe solve races
  let numWins = map countWins solutions
  let answer = product numWins
  print answer

-- * part 2

parseSpacedNumber :: Parsec String () Int
parseSpacedNumber = read <$> many1 (digit <* spaces)

parseSingleRaceFile :: Parsec String () Race
parseSingleRaceFile =
  do
    t <- string "Time:" *> spaces *> parseSpacedNumber
    d <- string "Distance:" *> spaces *> parseSpacedNumber
    return MkRace {time = t, dist = d}

solveAndUnpack :: Race -> (Float, Float)
solveAndUnpack race = case solve race of
  Nothing -> error "race has no solutions"
  (Just x) -> x

part2 :: Solution
part2 t = do
  content <- readFile t
  let race = parse content parseSingleRaceFile
  let solution = countWins $ solveAndUnpack race
  print race
  print solution
