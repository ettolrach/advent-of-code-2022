module Main where
import Data.List ( sortBy )

-- In order to avoid importing all of Data.List.Split, just define the split function that's needed.
splitOnEmpty :: String -> [String] -> [[String]]
splitOnEmpty _ [""] = []
splitOnEmpty c s =
  cons
    ( case break (== c) s of
        (l, s') ->
          ( l,
            case s' of
              [] -> []
              _ : s'' -> splitOnEmpty c s''
          )
    )
  where
    cons ~(h, t) = h : t

main :: IO ()
main = interact func

-- This will sum each elf's calories, sort them in descending order, and sum the top three (and convert it to a string).
func :: String -> String
func input = show $ sum $ take 3 $ sortBy (flip compare) $ map sum (elves input)

-- This is a list of the list of calories each elf has.
elves :: String -> [[Int]]
elves input = map (map (read :: String -> Int)) $ splitOnEmpty "" (lines input)
