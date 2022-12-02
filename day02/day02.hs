module Main where

data Move = R | P | S
    deriving (Eq, Show)

instance Ord Move where
    compare P R = GT
    compare P S = LT
    compare P P = EQ
    compare R S = GT
    compare R P = LT
    compare R R = EQ
    compare S P = GT
    compare S R = LT
    compare S S = EQ

type Round = (Move, Move)

type Outcome = Ordering
-- LT = loss, EQ = draw, GT = win.

main :: IO ()
main = interact func

func :: String -> String
func input = part1 input ++ "\n" ++ part2 input ++ "\n"

part1 :: String -> String
part1 input = show $ sum $ map evaluateRoundP1 (rounds1 input)

part2 :: String -> String
part2 input = show $ sum $ map evaluateRoundP2 (rounds2 input)

-- Get rounds from the input file.
rounds1 :: String -> [Round]
rounds1 input = map toRound (lines input)
  where
    toRound :: String -> Round
    -- This is an incomplete pattern match, but it's okay since the input format is strictly known.
    toRound (opponent : space : you : xs) = (charToMove opponent, charToMove you)

rounds2 :: String -> [(Move, Ordering)]
rounds2 input = map toRound (lines input)
  where
    toRound :: String -> (Move, Ordering)
    toRound (opponent : space : outcome : xs) = (charToMove opponent, charToOutcome outcome)

-- Again, incomplete pattern match, but it's okay.
charToMove :: Char -> Move
charToMove 'A' = R
charToMove 'B' = P
charToMove 'C' = S
charToMove 'X' = R
charToMove 'Y' = P
charToMove 'Z' = S

charToOutcome :: Char -> Outcome
charToOutcome 'X' = LT
charToOutcome 'Y' = EQ
charToOutcome 'Z' = GT

score :: Move -> Outcome -> Int
score m o = baseScore m + outcomeScore o
  where
    baseScore :: Move -> Int
    baseScore R = 1
    baseScore P = 2
    baseScore S = 3

    outcomeScore :: Outcome -> Int
    outcomeScore LT = 0
    outcomeScore EQ = 3
    outcomeScore GT = 6

evaluateRoundP1 :: Round -> Int
evaluateRoundP1 (a, b) = score b outcome
  where
    outcome :: Outcome
    outcome = compare b a

evaluateRoundP2 :: (Move, Outcome) -> Int
evaluateRoundP2 (m, o) = score response o
  where
    response :: Move
    response = findResponse (m, o)

findResponse :: (Move, Outcome) -> Move
-- Find out which move to make so that the desired outcome occurs.
-- In other words, find whether R, P, or S gives o when using the compare function.
findResponse (m, o) = head [r | r <- [R, P, S], compare r m == o]
