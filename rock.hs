import System.Random
import Data.Char
import Data.Sort
import Data.List
import System.IO
data Hand = Invalid | Rock | Paper | Scissors deriving (Show, Eq)
-- Defining my own instance of Ord, otherwise it would be wrong
-- (Considering how deriving Ord, it compares first letters, so P < R)
instance Ord Hand where
  compare Rock Paper = LT
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare Invalid _ = EQ -- To make it properly full instance
  compare x        y
    | x == y    = EQ
    | otherwise = flipOrder $ compare y x
flipOrder :: Ordering -> Ordering
flipOrder LT = GT
flipOrder GT = LT
flipOrder EQ = EQ
-- abstract types a bit for aesthics and clarity
type RandInt = IO Int
type Result = IO ()
type Weight = Int
-- Roll random number, between 1 and 3
rollNum :: RandInt
rollNum = getStdRandom (randomR (1,3))
-- Start of weightened rolls for 'AI'
weightRoll :: Weight -> RandInt
weightRoll a = rollNum >>= \x ->
  if x /= a then rollNum else return x
-- Opponent's hand choice based on the RNG defined earlier
handOpponent :: Int -> Hand
handOpponent x =
  case x of
    1 -> Rock
    2 -> Paper
    3 -> Scissors
-- Reverse of previous function, for weightened rolls
numHand :: Hand -> Int
numHand hand = case hand of
  Rock -> 1
  Paper -> 2
  Scissors -> 3
-- Function that takes Hand as argument, and enemy roll , and compares your choice with opponent's
--      and then subsequently use it
handCompare :: Hand -> Hand -> Result
handCompare choice opChoice =
  let enemyStr = "Enemy's hand is: " ++ show opChoice in
    putStrLn $ case compare choice opChoice  of
      EQ -> enemyStr ++ ", Draw"
      LT -> enemyStr ++ ", Loss"
      GT -> enemyStr ++ ", Win"

-- Write score
writeHandCompare :: Hand -> Hand -> Result
writeHandCompare choice opChoice =
  let write = appendFile "Score.txt" in
    case compare choice opChoice  of
      EQ -> write "Draw\n"
      LT -> write "Loss\n"
      GT -> write "Win\n"

-- Keep track of user choice
writeHand :: Hand -> IO ()
writeHand choice =
  let write = appendFile ".hand.txt" in
    let newLine a = a ++ "\n" in
      write $ newLine $ show $ numHand choice

-- helper function
longerList :: Ord a => [a] -> [a] -> [a]
longerList a b =
  case compare (length a) (length b) of
    EQ -> a
    LT -> b
    GT -> a
-- read hand
readHand :: IO Int
readHand = do
  handle <- readFile ".hand.txt"
  let intList x = map read $ lines x in
    let findBig x = group $ sort x in
      return (head $ foldr longerList [] (findBig $ intList handle))
-- Full function, converst String to Hand data
ioToHand :: String -> Hand
ioToHand choice = case map toLower choice of
  "rock" ->  Rock
  "scissors" ->  Scissors
  "paper" ->  Paper
  _ -> Invalid
-- function that calls specific functions,
-- Basically, read from Input,
-- Use it as Hand, compare it,
-- And then,
-- If its invalid, print invalid, and end,
-- else, make the play, write the score, and call main again
playRound :: Hand -> IO ()
playRound enemy =
  putStrLn "Choose Rock, Scissors or paper" >>
  getLine >>= \choice ->
  if ioToHand choice == Invalid
    then putStrLn "Invalid"
    else handCompare (ioToHand choice) enemy >>
         writeHandCompare (ioToHand choice) enemy >>
         writeHand (ioToHand choice) >>
         main
-- said main fn
main :: IO ()
main =
  readHand >>= \weight ->
  weightRoll weight >>= \roll ->
  playRound (handOpponent roll)
