module Day10B where

import Control.Monad.State.Strict
  ( State,
    evalState,
    gets,
    modify,
  )
import Data.List (sort)
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- readFile "input/Day10.txt"
  let preparedInput = allAdapters $ parseInput input
  print $ evalState (stateMemoPerms preparedInput) Map.empty

-- I went for the ugly solution from this gist:
-- https://gist.github.com/beala/d871ae8397167e7035f218a25ddf87dd
-- I tried, but could not get a lazy memoization implementation to work in
-- combination with a Map :(.
-- At least I find it easy to understand what is happening here.
stateMemoPerms :: [Int] -> State (Map.Map [Int] Int) Int
stateMemoPerms [] = return 0
stateMemoPerms (x : y : z : xs)
  | z - x > 3 = getOrUpdate (y : z : xs) (stateMemoPerms (y : z : xs))
  | otherwise = do
    n1 <- getOrUpdate (y : z : xs) (stateMemoPerms (y : z : xs))
    n2 <- getOrUpdate (x : z : xs) (stateMemoPerms (x : z : xs))
    return (n1 + n2)
stateMemoPerms (_ : _) = return 1

getOrUpdate :: (Ord k) => k -> State (Map.Map k v) v -> State (Map.Map k v) v
getOrUpdate k ifEmptyState = do
  maybeVal <- gets (Map.lookup k)
  case maybeVal of
    Just v -> return v
    Nothing -> do
      ifEmpty <- ifEmptyState
      modify (Map.insert k ifEmpty)
      return ifEmpty

allAdapters :: [Int] -> [Int]
allAdapters xs = 0 : sort xs ++ [maximum xs + 3]

parseInput :: String -> [Int]
parseInput = map read . lines
