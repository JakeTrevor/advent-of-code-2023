module Day2
  ( part1,
    part2,
  )
where

import Control.Monad (void)
import Lib (Solution)
import Text.Parsec (ParseError, Parsec, choice, digit, getState, many1, optional, putState, runParser, spaces, string)

data Game = MkGame {gameID :: Int, red :: Int, blue :: Int, green :: Int}

instance Show Game where
  show (MkGame gid r b g) = "Game " ++ show gid ++ " r:" ++ show r ++ " b:" ++ show b ++ " g:" ++ show g

data Colour = Red | Green | Blue

lineHead :: Parsec String Game ()
lineHead =
  do
    _ <- string "Game "
    d <- many1 digit
    putState $ MkGame (read d) 0 0 0
    _ <- string ": "
    return ()

singleDraw :: Parsec String Game ()
singleDraw =
  do
    count <- many1 digit
    spaces
    hue <-
      choice
        [ string "green" >> return Green,
          string "blue" >> return Blue,
          string "red" >> return Red
        ]
    state <- getState
    putState $ getNewState state hue (read count)
    optional (string ", ")
    return ()

bagDraw :: Parsec String Game ()
bagDraw = do
  singleDraw
  optional singleDraw
  optional singleDraw
  optional (string "; ")
  return ()

getNewState :: Game -> Colour -> Int -> Game
getNewState g Red c
  | red g < c = g {red = c}
  | otherwise = g
getNewState g Blue c
  | blue g < c = g {blue = c}
  | otherwise = g
getNewState g Green c
  | green g < c = g {green = c}
  | otherwise = g

game :: Parsec String Game ()
game = do
  lineHead
  void (many1 bagDraw)

parseLine :: String -> Either ParseError Game
parseLine = runParser (game >> getState) (MkGame 0 0 0 0) ""

errorIfEvil :: Either ParseError Game -> Game
errorIfEvil (Right g) = g
errorIfEvil (Left e) = error $ show e

checkGame :: Game -> Game -> Bool
checkGame ok g =
  red g <= red ok
    && blue g <= blue ok
    && green g <= green ok

part1 :: Solution
part1 f = do
  let check = checkGame $ MkGame (-1) 12 14 13
  content <- readFile f
  let lns = lines content
  let allGames = map (errorIfEvil . parseLine) lns
  let okGames = filter check allGames
  let ids = map gameID okGames

  print $ sum ids

computePower :: Game -> Int
computePower (MkGame _ r b g) = r * b * g

part2 :: Solution
part2 f = do
  content <- readFile f
  let lns = lines content
  let allGames = map (errorIfEvil . parseLine) lns

  let powers = map computePower allGames

  print $ sum powers
