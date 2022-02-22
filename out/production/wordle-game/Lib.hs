{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Lib
    ( startFunc
    ) where
      
import System.IO
import Graphics.Gloss
import Data.Char (toUpper)
import Control.Exception.Base (IOException, try)
import Constants
import Gameplay
import World

{-
  Функция получает начальное сообщение,
  далее ожидает путь к файлу с списком 
  возможных слов, если файл получается открыть,
  то вызывается функция launchGame, если такого
  файла не существует то функция ловит ошибку
  и запрашивает путь к файлу еще раз вызывая саму себя  
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
      Right val -> launchGame val chooseWordMessage

window :: Display
window = InWindow title (1500, 800) (0, 0) 

{-
  Функция получает список возможных слов из файла и
  сообщение, сообщение выводится в консоль,
  а с помощью списока запускается игра, так же
  в этом блоке считвается слово из списка, 
  которое и будет задуманным  
-}
launchGame :: String -> String -> IO ()
launchGame val m = do
  putStrLn m
  chosenWord <- getLine
  let cw = map toUpper chosenWord
  if cw `elem` myList
    then
      play 
        window 
        backgroundColor 
        30 
        (initialGamePane myList cw) 
        (drawGamePane cw) 
        handleInput 
        (const id)
    else
      launchGame val notInListException
      where myList = lines $ map toUpper val
