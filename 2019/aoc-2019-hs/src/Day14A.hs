module Day14A where

import qualified Data.Map as Map
import Data.List.Split ( splitOn )

data Chemical = Chemical String | Ore | Fuel deriving ( Show, Eq, Ord )

type ChemicalQuantity = (Chemical, Int)

type ChemicalQuantityList = Map.Map Chemical Int

type Reaction = ([ChemicalQuantity], ChemicalQuantity)

type ReactionList = [Reaction]

solve :: IO ()
solve = do
    input <- readFile "input/Day14.txt"
    let reactions = parseInput input
        requiredOre = getRequiredOre reactions
    print requiredOre

example0 :: String
example0 = "10 ORE => 10 A\n\
\1 ORE => 1 B\n\
\7 A, 1 B => 1 C\n\
\7 A, 1 C => 1 D\n\
\7 A, 1 D => 1 E\n\
\7 A, 1 E => 1 FUEL"

getRequiredOre :: ReactionList -> [ChemicalQuantity]
getRequiredOre reactions = Map.toList $ go reactions (Map.fromList [(Fuel, 1)]) Map.empty
    where
        go :: ReactionList -> ChemicalQuantityList -> ChemicalQuantityList -> ChemicalQuantityList
        go reactions need have
            | isAllOre need = need
            | otherwise =
                let (chemical, amount) = getFirstNonOre need
                    reaction = findReactionWithResult chemical reactions
                    (requiredQuantities, amountGenerated) = getRequiredQuantitiesForAmount reaction amount
                    (newNeed, newHave) = resolve requiredQuantities need have
                    newHaveWithResult = add newHave (chemical, amountGenerated - amount)
                    newNeedWithoutResult = remove newNeed chemical
                --in go reactions newNeed newHaveWithResult
                in go reactions newNeedWithoutResult newHaveWithResult

-- |Returns the required inputs for a given reaction such that it generates at
-- least the given amount of output. The actual generated amount is returned
-- as well.
getRequiredQuantitiesForAmount :: Reaction -> Int -> ([ChemicalQuantity], Int)
getRequiredQuantitiesForAmount (inputs, (_, outputAmount)) requiredAmount =
    let multAmount = ceiling (fromIntegral requiredAmount / fromIntegral outputAmount)
    in (map (mult multAmount) inputs, multAmount * outputAmount)

isAllOre :: ChemicalQuantityList -> Bool
isAllOre m = Map.size m == 1 && Map.member Ore m

getFirstNonOre :: ChemicalQuantityList -> ChemicalQuantity
getFirstNonOre m =
    let c = head $ Map.keys m
    in (c, (Map.!) m c)

-- |Takes a list of new quantities to resolve, the current list of needed
-- quantities, and the list of the current surplus quantities. Resolves the new
-- quantities by first removing from the surplus and then adding what's left to
-- the list of needed quantities.
resolve :: [ChemicalQuantity]
    -> ChemicalQuantityList
    -> ChemicalQuantityList
    -> (ChemicalQuantityList, ChemicalQuantityList)
resolve new need have =
    foldr resolveSingle (need, have) new
    where
        resolveSingle :: ChemicalQuantity
            -> (ChemicalQuantityList, ChemicalQuantityList)
            -> (ChemicalQuantityList, ChemicalQuantityList)
        resolveSingle (c,n) (need, have) =
            case get have c of
                Just (_, n') ->
                    -- We have more surplus than required
                    if n < n'
                        then (need, sub have (c, n))
                        -- We use up the surplus and have
                        -- a required amount left
                        else (add need (c, n - n'), remove have c)
                Nothing -> (add need (c,n), have)

remove :: ChemicalQuantityList -> Chemical -> ChemicalQuantityList
remove m c = Map.delete c m

sub :: ChemicalQuantityList -> ChemicalQuantity -> ChemicalQuantityList
sub m (c,n) =
    case get m c of
        Just (c, 0) -> remove m c
        Just (c, _) -> add m (c, -n)
        Nothing -> m

add :: ChemicalQuantityList -> ChemicalQuantity -> ChemicalQuantityList
add m (_, 0) = m
add m (c,n) = Map.insertWith (+) c n m

get :: ChemicalQuantityList -> Chemical -> Maybe ChemicalQuantity
get m c = case Map.lookup c m of
            Just n -> Just (c, n)
            _ -> Nothing

mult :: Int -> ChemicalQuantity -> ChemicalQuantity
mult n (c, amount) = (c, n * amount)

findReactionWithResult :: Chemical -> ReactionList -> Reaction
findReactionWithResult c [] = error $ "Reaction with " ++ show c ++ " as result not found"
findReactionWithResult c (x:xs) | (fst . snd) x == c = x
                                | otherwise = findReactionWithResult c xs

findFuelReaction :: ReactionList -> Reaction
findFuelReaction = findReactionWithResult Fuel

-- Parsing

parseInput :: String -> ReactionList
parseInput = map parseLine . lines

parseLine :: String -> Reaction
parseLine input = (inputs, output)
    where
        [inputStr, outputStr] = splitOn " => " input
        output = parseChemicalQuantity outputStr
        inputs = (map parseChemicalQuantity . splitOn ", ") inputStr


-- \ E.g. "7 A" => (Chemical "A", 7)
parseChemicalQuantity :: String -> ChemicalQuantity
parseChemicalQuantity input = (chemical, quantity)
    where
        [quantityString, chemicalId] = splitOn " " input
        chemical = parseChemical chemicalId
        quantity = read quantityString

parseChemical :: String -> Chemical
parseChemical "ORE" = Ore
parseChemical "FUEL" = Fuel
parseChemical id = Chemical id
