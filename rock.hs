{-# LANGUAGE LambdaCase #-}
import System.Random
import Data.Char
data Hand = Invalid | Rock | Paper | Scissors deriving (Show, Eq)
-- Defining my own instance of Ord, otherwise it would be wrong
-- (Considering how deriving Ord, it compares first letters, so P < R)
instance Ord Hand where
  compare Rock Paper = LT
  compare Rock Scissors = GT
  compare Rock Rock = EQ
  compare Paper Rock = GT
  compare Paper Paper = EQ
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare Scissors Paper = GT
  compare Scissors Scissors = EQ
-- Roll random number, between 1 and 3
rollNum :: IO Int
rollNum = getStdRandom (randomR (1,3))
-- Opponent's hand choice based on the RNG defined earlier
handOpponent :: IO Hand
handOpponent = rollNum >>=
  \case
    1 -> return Rock
    2 -> return Paper
    3 -> return Scissors
-- Function that takes Hand as argument, calls handOpponent, and compares your choice with opponent's
handCompare :: Hand -> IO ()
handCompare choice =
  handOpponent >>= (\x -> let enemyStr = "Enemy's hand is: " ++ show x in
                      case compare choice x of
                                        EQ -> putStrLn (enemyStr ++ ", Draw")
                                        LT -> putStrLn (enemyStr ++ ", Loss")
                                        GT -> putStrLn (enemyStr ++ ", Win"))
-- Full function, converst String to Hand data
ioToHand :: String -> Hand
ioToHand choice = case map toLower choice of
  "rock" ->  Rock
  "scissors" ->  Scissors
  "paper" ->  Paper
  _ -> Invalid
-- main function that calls specific functions
playRound :: IO ()
playRound =
  putStrLn "Choose Rock, Scissors or paper" >>
  getLine >>= \choice ->
  if ioToHand choice == Invalid
    then putStrLn "Invalid"
    else handCompare  (ioToHand choice) >>
         main

main :: IO ()
main =
  playRound
