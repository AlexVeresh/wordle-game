{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Lib
    ( startFunc
    ) where
      
import System.IO
import Graphics.Gloss
import Data.Char (toUpper)
import System.Random
import Control.Exception.Base (IOException, try)
import Constants
import Gameplay
import World

{-
  The function receives the initial message,
  then expects the path to the file with the list
  of possible words. If the file can be opened,
  then the launchGame function is called, if such
  file does not exist, the function catches an error
  and asks for the path to the file by calling 
  itself again  
-}
startFunc :: String -> IO ()
startFunc intro = do
  hSetBuffering stdout NoBuffering
  putStrLn intro
  path <- getLine
  file <- try (readFile path) :: IO (Either IOException String)
  case file of
      Left _ -> do 
        putStrLn noFileException
        startFunc tryAgainMessage
      Right val -> launchGame val

window :: Display
window = InWindow title (1500, 800) (0, 0) 

{-
  The function gets a list of possible words from the file and
  message, the message is printed to the console,
  and the game is launched with the list of words, also
  in this block function chooses a random word from the list
  as an intended word  
-}
launchGame :: String -> IO ()
launchGame val = do
  randomIdx <- randomRIO(0, length myList-1)
  let chosenWord = myList !! randomIdx
  putStr (chosenWordMessage ++ chosenWord)
  play 
    window 
    backgroundColor 
    30 
    (initialGamePane myList chosenWord) 
    (drawGamePane chosenWord) 
    handleInput 
    (const id)
  where myList = lines $ map toUpper val
