module Data where

import Graphics.Gloss
  
data KeyTile = ENTER | DELETE | KeyTile Char
data GameResult = WIN | DEFEAT | PLAY | GameResult String 
data KeyboardButton = KeyboardButton {
    key :: String, 
    position :: (Float, Float), 
    col :: Color
  }
data GameTile = GameTile {myVal :: Maybe Char, trueVal :: Char, hidden :: Bool}
data GamePane = GamePane {
    wordList :: [String],
    word :: String,
    currentWord :: String,
    currentTurn :: Int,
    currentPos :: Int, 
    tiles :: [GameTile], 
    inputInterface :: [KeyboardButton],
    gameResult :: GameResult
  }
