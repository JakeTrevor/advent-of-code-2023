module Main (main) where

import Day6 (part1, part2)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let f = head args
  part2 f
