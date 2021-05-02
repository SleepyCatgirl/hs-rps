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

--handCompare choice =
--  choice >>= (\y -> handOpponent >>= (\x -> case compare y x of
--                                        EQ -> putStrLn ("Enemy's hand is: " ++ (show x) ++ ", Draw")
--                                        LT -> putStrLn ("Enemy's hand is: " ++ (show x) ++ ", Loss")
--                                        GT -> putStrLn ("Enemy's hand is: " ++ (show x) ++ ", Win")))
-- Since getLine results in [Char], lets define function that converts [Char] -> Hand
-- Maybe to see case where we write something else than choice
ioToHand :: [Char] -> Maybe Hand
ioToHand choice = case choice of
  "Rock" -> Just Rock
  "rock" -> Just Rock
  "Scissors" -> Just Scissors
  "Scissors" -> Just Scissors
  "scissors" -> Just Scissors
  "Paper" -> Just Paper
  "paper" -> Just Paper
  otherwise -> Nothing
-- main function that calls specific functions
main =
  putStrLn "Choose Rock, Scissors or paper" >>
  getLine >>= \choice ->
-- I dont know how to make a function that uses iotoHand that is full
  if ioToHand choice == Nothing then putStr "Nya" else putStr "Nya"
  --(ioToHand choice >>= handCompare)
    --ioToHand choice handCompare
