module London where

import System.IO
import Data.List.Split (chunksOf)

main = do
  input <- readFile "input.txt"
  let weights = parseInput input
      path = getShortestPath weights in
    putStr $ show path

parseInput :: [Char] -> [Int]
parseInput = map (read :: String->Int) . lines

-- left :: -1
-- right :: 1

getShortestPath :: (Num a, Ord a) => [a] -> [a]
getShortestPath (x:y:xs) =
  let acc = if x > y then [-1, y] else [1, x]
      list3 = chunksOf 3 xs
  in foldl evalShortest acc list3

evalShortest :: (Num a, Ord a) => [a] -> [a] -> [a]
evalShortest (-1:acc) (c:x:y:[])
  | c + x < y = 1:x:c:acc
  | otherwise = -1:y:acc
evalShortest (1:acc) (c:x:y:[])
  | c + y < x = -1:y:c:acc
  | otherwise = 1:x:acc
