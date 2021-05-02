import System.Random
data Hand = Rock | Paper | Scissors deriving (Show, Eq)
-- Defining my own instance of Ord, otherwise it would be wrong (Considering how deriving Ord, it compares first letters, so P < R)
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
handOpponent = rollNum >>= (
  \x -> case x of
    1 -> return Rock
    2 -> return Paper
    3 -> return Scissors
                           )
-- Function that takes Hand as argument, calls handOpponent, and compares your choice with opponent's
handCompare choice =
  handOpponent >>= (\x -> case compare choice x of
                                        EQ -> putStrLn ("Enemy's hand is: " ++ (show x) ++ ", Draw")
                                        LT -> putStrLn ("Enemy's hand is: " ++ (show x) ++ ", Loss")
                                        GT -> putStrLn ("Enemy's hand is: " ++ (show x) ++ ", Win"))
  -- a lot of repettion, so lets use where
-- Full function, converst String to Hand data
ioToHand :: [Char] -> Maybe Hand
ioToHand choice = case choice of
  "Rock" -> Maybe Rock
  "rock" -> Maybe Rock
  "Scissors" -> Maybe Scissors
  "Scissors" -> Maybe Scissors
  "scissors" -> Maybe Scissors
  "Paper" -> Maybe Paper
  "paper" -> Maybe Paper
  _       -> Nothing
magic x = case x of
  Just x -> handCompare x
  Nothing -> putStrLn "Nothing"
-- main function that calls specific functions
main =
  putStrLn "Choose Rock, Scissors or paper" >>
  getLine >>= \choice -> magic (ioToHand choice)
