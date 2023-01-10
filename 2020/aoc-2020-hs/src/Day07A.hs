module Day07A where

import Data.Graph.DGraph (DGraph, fromArcsList)
import Data.Graph.Types (Arc (..))
import Data.Graph.Visualize
import Data.List.Split (chunksOf)

type BagType = String

main :: IO ()
main = undefined

inputToDGraph :: String -> DGraph String Int
inputToDGraph = fromArcsList . concatMap parseLine . lines

-- It's ugly. But it works.
parseLine :: String -> [Arc String Int]
parseLine input
  | unwords (drop 3 $ words input) == "contain no other bags." = []
  | otherwise = map (uncurry $ Arc container) containedBags
  where
    -- Instead of regexes/parsing, let's just drop every word except
    -- bag-related stuff.
    bagWords = filter (`notElem` ["bag", "bag.", "bag,", "bags", "bags.", "bags,", "contain", "contains"]) $ words input

    -- The bag that contains the other bags (just the color name).
    container = unwords $ take 2 bagWords

    -- A list of quantities & bag names that are contained in the main bag.
    containedBags =
      map (\[n, b1, b2] -> (unwords [b1, b2], read n)) $
        chunksOf 3 $
          drop 2 bagWords

exampleInput :: String
exampleInput =
  unlines
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags.",
      "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
      "bright white bags contain 1 shiny gold bag.",
      "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
      "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
      "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
      "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
      "faded blue bags contain no other bags.",
      "dotted black bags contain no other bags."
    ]
