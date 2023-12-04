module Day3
  ( part1,
    part2,
  )
where

import Data.Char (isDigit)
import Lib (Solution)

data NumRef = MkNum {val :: String, x_pos :: Int, y_pos :: Int}

remove :: Int -> [a] -> [a]
remove 0 x = x
remove _ [] = []
remove i (_ : xs) = remove (i - 1) xs

grabNumber :: String -> String
grabNumber [] = []
grabNumber (x : xs)
  | isDigit x = x : grabNumber xs
  | otherwise = ""

grabAllNumbers :: Int -> Int -> String -> [NumRef]
grabAllNumbers x y ('.' : xs) = grabAllNumbers (x + 1) y xs
grabAllNumbers x y (p : ps)
  | isDigit p = i : grabAllNumbers x' y rest
  | otherwise = grabAllNumbers (x + 1) y ps
  where
    val = grabNumber (p : ps)
    x' = x + length val + 1
    rest = remove (length val) ps
    i = MkNum {val = val, x_pos = x, y_pos = y}
grabAllNumbers _ _ [] = []

checkNum :: [String] -> NumRef -> Bool
checkNum lns n = any (checkCoord lns) coords
  where
    coords = mkCoords n

mkCoords :: NumRef -> [(Int, Int)]
mkCoords (MkNum v x y) =
  [(x - 1, y), (x + length v, y)]
    ++ concat [[(i, y - 1), (i, y + 1)] | i <- [x - 1 .. x + length v]]

checkCoord :: [String] -> (Int, Int) -> Bool
checkCoord [] _ = False
checkCoord lns (x, y)
  | inRange = not (c == '.' || isDigit c)
  | otherwise = False
  where
    inRange = (x >= 0) && (y >= 0) && (y < length lns) && (x < length (head lns))
    ln = lns !! y
    c = ln !! x

part1 :: Solution
part1 t = do
  content <- readFile t
  let lns = lines content
  let nums = concat [grabAllNumbers 0 i (lns !! i) | i <- [0 .. length lns - 1]]
  let valid = filter (checkNum lns) nums
  let vs = map (read . val) valid
  print (sum vs)

-- * part 2

grabAllStars :: Int -> Int -> String -> [NumRef]
grabAllStars x y ('*' : xs) = MkNum {val = "*", x_pos = x, y_pos = y} : grabAllStars (x + 1) y xs
grabAllStars x y (_ : xs) = grabAllStars (x + 1) y xs
grabAllStars _ _ [] = []

makeGear :: [NumRef] -> NumRef -> (Int, Int)
makeGear nums star
  | valid = (read (val a), read (val b))
  | otherwise = (0, 0)
  where
    adjacents = filter (inRange star) nums
    valid = length adjacents == 2
    a : b : _ = adjacents

inRange :: NumRef -> NumRef -> Bool
inRange star t = any f t_spots
  where
    (x, y) = (x_pos star, y_pos star)
    star_spots = [(i, j) | i <- [x - 1 .. x + 1], j <- [y - 1 .. y + 1]]
    t_spots = [(i, y_pos t) | i <- [x_pos t .. x_pos t + length (val t) - 1]]
    f e = e `elem` star_spots

part2 :: Solution
part2 t = do
  content <- readFile t
  let lns = lines content
  let nums = concat [grabAllNumbers 0 i (lns !! i) | i <- [0 .. length lns - 1]]
  let stars = concat [grabAllStars 0 i (lns !! i) | i <- [0 .. length lns - 1]]

  let gears = map (makeGear nums) stars
  let ratios = map (uncurry (*)) gears
  print $ sum ratios
