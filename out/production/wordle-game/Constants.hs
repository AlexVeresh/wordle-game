module Constants where

import Graphics.Gloss

backgroundColor, rightLetterColor, rightPosColor, wrongLetterColor, keyTileColor :: Color
backgroundColor = makeColorI 255 255 255 255
rightLetterColor = makeColorI 201 180 88 255 
rightPosColor = makeColorI 106 170 100 255 
wrongLetterColor = makeColorI 120 124 126 255
keyTileColor = makeColorI 206 212 218 255

keyTileWidth, keyTileHeight, keyTileRadius, gameTileSize, tileMargin:: Float
keyTileWidth = 42
keyTileHeight = 48
keyTileRadius = 8
gameTileSize = 58
tileMargin = 5

wordLength :: Int
wordLength = 5
            
introMessage, restartMessage, winMessage, defeatMessage :: String 
notInListMessage, chosenWordMessage:: String 
tryAgainMessage, noFileException, enter, delete, title :: String
introMessage 
  = "Hi! Before we start, enter the path to the text file where all possible words lie:"
restartMessage = "Press F5 to restart the game"
winMessage = "Great! This word is correct"
defeatMessage = "Correct word was "
notInListMessage = "This word not in word list"
chosenWordMessage = "Random chosen word is "
tryAgainMessage = "Try again:"
noFileException = "No such file or directory"
enter = "ENTER"
delete = "DELETE"
title = "Wordle"

keyboard :: [String]
keyboard = ["Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P", 
            "A", "S", "D", "F", "G", "H", "J", "K", "L", enter,
            "Z", "X", "C", "V", "B", "N", "M", delete]           