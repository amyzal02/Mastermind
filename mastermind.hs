import System.Random (randomRIO)
import Control.Monad (replicateM)

data Color = Red | Green | Blue | Yellow | Orange | Purple deriving (Show, Eq, Enum)
type Code = [Color]
codeLength :: Int
codeLength = 4


-- (The Bounded type class is used for any type that 
--  has a min and max value, it is useful for my code to 
-- easily get random values of any color)
instance Bounded Color where
  minBound = Red
  maxBound = Purple

-- used to store guess results 
data GuessResult = GuessResult
  { guess :: Code
  , correctPosition :: Int
  , correctColor :: Int
  }

-- Function to generate a random secret code
-- uses replicateM to create a list of random colors of codeLength length. 
generateSecretCode :: IO Code
generateSecretCode = replicateM codeLength getRandomColor

-- Helper function for generateSecretCode, 
-- generates a random color for each individual peg 
getRandomColor :: IO Color
getRandomColor = do
  index <- randomRIO (fromEnum (minBound :: Color), fromEnum (maxBound :: Color))
  return (toEnum index)

-- Function to get user input for a guess
getUserGuess :: Code -> IO Code
getUserGuess secretCode = do
  putStrLn "Enter your guess (e.g., RGBY) or 'quit' to end the game: "
  input <- getLine
  case input of
    "cheat" -> do
      putStrLn $ "Secret Code: " ++ show secretCode
      getUserGuess secretCode
    "quit" -> do
      pure []
    _ -> do
      let maybeGuess = mapM colorFromCode input
      case maybeGuess of
        Just guess ->
          if length guess == codeLength
            then return guess
            else do
              putStrLn "Invalid guess. Please enter a valid guess."    
              getUserGuess secretCode
        Nothing -> do
          putStrLn "Invalid guess. Please enter a valid guess."
          getUserGuess secretCode

-- Function to convert a character to a color
colorFromCode :: Char -> Maybe Color
colorFromCode 'R' = Just Red
colorFromCode 'G' = Just Green
colorFromCode 'B' = Just Blue
colorFromCode 'Y' = Just Yellow
colorFromCode 'O' = Just Orange
colorFromCode 'P' = Just Purple
colorFromCode 'r' = Just Red
colorFromCode 'g' = Just Green
colorFromCode 'b' = Just Blue
colorFromCode 'y' = Just Yellow
colorFromCode 'o' = Just Orange
colorFromCode 'p' = Just Purple
colorFromCode _   = Nothing

-- Function to evaluate a guess and provide feedback:
-- Explanation: returns a tuple of two Ints: correctPosition and correctColor 
-- The function uses a zip to pair those, filters them to see if they overlap
-- and counts the total correct colors 
evaluateGuess :: Code -> Code -> (Int, Int)
evaluateGuess secret guess =
  let correctPosition = length $ filter (\(x, y) -> x == y) (zip secret guess)
      correctColor = sum $ map (\color -> min (countColor secret color) (countColor guess color)) [Red .. Purple]
  in (correctPosition, correctColor - correctPosition)

-- Helper function to count occurrences of a color in a code
countColor :: Code -> Color -> Int
countColor code color = length $ filter (== color) code

  -- Function to display a single guess w/ its result
displayGuesses :: GuessResult -> IO ()
displayGuesses (GuessResult guess correctPosition correctColor) = do
  putStrLn $ "Guess: " ++ show guess ++ " | Result: " ++ show correctPosition ++ " Correct, " ++ show correctColor ++ " Partially Correct"


-- Function to display past guesses w/ results
pastGuesses :: [GuessResult] -> IO ()
pastGuesses guesses = do
  putStrLn ""
  putStrLn "Guesses:"
  mapM_ displayGuesses guesses
  putStrLn ""

-- Main game loop
play :: Code -> [GuessResult] -> IO ()
play secretCode pastGuessesList = do
  guess <- getUserGuess secretCode
  if null guess
    then do
      pastGuesses pastGuessesList
      putStrLn $ "Secret Code: " ++ show secretCode
      putStrLn " "
      putStrLn "Quitting the game. Goodbye!"
    else do
      let (correctPosition, correctColor) = evaluateGuess secretCode guess
      let newGuessResult = GuessResult guess correctPosition correctColor
      pastGuesses (pastGuessesList ++ [newGuessResult])
      if correctPosition == codeLength
        then putStrLn "Congratulations! You've guessed the secret code."
        else play secretCode (pastGuessesList ++ [newGuessResult])

-- Function to display an introduction w/ color options and rules
intro :: IO ()
intro = do
  putStrLn " "
  putStrLn "Welcome to Mastermind!"
  putStrLn " "
  putStrLn "Color Options: R (Red), G (Green), B (Blue), Y (Yellow), O (Orange), P (Purple)"
  putStrLn " "
  putStrLn "Rules: Try to guess the secret code, consisting of four colors chosen from the options above."
  putStrLn " "
  putStrLn "For each guess, you will receive feedback on the folllowing:" 
  putStrLn "  -# of completely correct colors (colors that are correctly in their space)"
  putStrLn "  -# of colors that are partially corect (colors that are in the secret code but not in the correct space)"
  putStrLn " "
  putStrLn "Type 'quit' to end the game at any time."
  putStrLn " "

main :: IO ()
main = do
  intro
  secretCode <- generateSecretCode
  play secretCode []