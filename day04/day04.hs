module Main where

import Data.List (isInfixOf, isSubsequenceOf)

type Assignment = (Int, Int)
type AssignmentPair = (Assignment, Assignment)
type AssignmentRange = [Int]
type AssignmentRangePair = ([Int], [Int])

main :: IO ()
main = interact func >> putStrLn ""

func :: String -> String
func input = part1 input ++ "\n" ++ part2 input

part1 :: String -> String
part1 = show . length . filter (\ (a, b) -> b `isInfixOf` a || a `isInfixOf` b) . pairsToRanges . makePairs

part2 :: String -> String
part2 = show . length . filter (/= []) . map (\ (a, b) -> filter (`elem` a) b) . pairsToRanges . makePairs

-- Modify the Prelude lines function to split on a generic item.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _  [] =  []
splitOn c xs = cons (case break (== c) xs of
    (l, xs') -> (l, case xs' of
        []      -> []
        _:xs''   -> splitOn c xs''))
  where
    cons ~(h, t)        =  h : t

makePairs :: String -> [AssignmentPair]
makePairs input = map (toTuple . map (\ (a, b) -> (read a :: Int, read b :: Int))) strAssignments
  where
    strAssignments :: [[(String, String)]]
    strAssignments = map (map (toTuple . splitOn '-') . splitOn ',') (lines input)
    -- This function assumes the input is correct!
    toTuple :: Show b => [b] -> (b, b)
    toTuple (a:b:xs) = (a, b)

pairsToRanges :: [AssignmentPair] -> [AssignmentRangePair]
pairsToRanges = map (\ (a, b) -> (makeRange a, makeRange b))

makeRange :: Assignment -> AssignmentRange
makeRange (a, b) = [a..b]
