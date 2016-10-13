module London2 where

import System.IO
import Data.List.Split (chunksOf)

----- Types -----

-- Types that define Paths between nodes
data Label = A | B | C deriving (Show)
type Cost = Int
type Path = [(Label, Cost)]
type MeasuredPath = (Path, Cost)

-- ADT for the whole RoadSystem and a Section
data Section = Section { getA :: Cost, getB :: Cost, getC :: Cost } deriving (Show)
type RoadSystem = [Section]

----- Constants -----

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

----- Main  -----

main = do
  input <- readFile "input.txt"
  let costs = parseInput input
      threes = chunksOf 3 costs
      roadSystem = map (\[a,b,c] -> Section a b c) threes in
      mapM print $ optimalPath roadSystem

----- Functions -----

parseInput :: [Char] -> [Int]
parseInput = map (read :: String->Int) . lines

optimalPath :: RoadSystem -> Path
optimalPath = reverse . shortestPath . foldl roadStep (initialA, initialB)
  where initialA = ([], 0)
        initialB = initialA

shortestPath :: (MeasuredPath, MeasuredPath) -> Path
shortestPath ((pathA, priceA), (pathB, priceB)) =
  if priceA < priceB then pathA else pathB

roadStep :: (MeasuredPath, MeasuredPath) -> Section -> (MeasuredPath, MeasuredPath)
roadStep ((pathA, costA), (pathB, costB)) (Section a b c) =
  let costForwardToA = costA + a
      costCrossToB = costA + c + b
      costForwardToB = costB + b
      costCrossToA = costB + c + a

      measuredPathA = if costForwardToA <= costCrossToB
                      then ((A, a):pathA, costForwardToA)
                      else ((C, c):(B, b):pathB, costCrossToB)

      measuredPathB = if costForwardToB <= costCrossToA
                      then ((B, b):pathB, costForwardToB)
                      else ((C, c):(A, a):pathA, costCrossToA)

  in (measuredPathA, measuredPathB)
