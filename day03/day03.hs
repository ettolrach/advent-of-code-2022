module Main where

import Data.Char (ord, isUpper)
import Debug.Trace (trace)

type Item = Char
type Compartment = [Item]
type Rucksack = (Compartment, Compartment)

main :: IO ()
main = interact func >> putStrLn ""

func :: String -> String
func input = part1 input ++ "\n" ++ part2 input

part1 :: String -> String
part1 input = show $ sum $ map (priority . findDuplicate) (rucksacks input)

part2 :: String -> String
part2 input = show $ sum $ map (priority . head . findDuplicatesFromList) (groups input)

splitCompartments :: String -> Rucksack
splitCompartments s = splitAt (length s `div` 2) s

rucksacks :: String -> [Rucksack]
rucksacks input = map splitCompartments (lines input)

groupList :: Int -> [a] -> [[a]]
groupList _ [] = []
groupList n l = take n l : groupList n (drop n l)

groups :: String -> [[String]]
groups input = groupList 3 (lines input)

findDuplicate :: Rucksack -> Item
findDuplicate (c1, c2) = (head . findDuplicatesFromList) [c1, c2]

findDuplicatesFromList :: Eq a => [[a]] -> [a]
findDuplicatesFromList [] = []
findDuplicatesFromList (x:xs) = filter (\ e -> all (e `elem`) xs) x

priority :: Char -> Int
priority c
    | isUpper c = ord c - 64 + 26
    | otherwise = ord c - 96
