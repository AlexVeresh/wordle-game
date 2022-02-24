module Gameplay where

import Data
import Constants  
import Graphics.Gloss.Interface.IO.Game

{-
  The function gets a list of possible words from a file,
  intended word and generates an object of the initial
  playing field  
-}
initialGamePane :: [String] -> String -> GamePane
initialGamePane l w = GamePane {
    wordList=l,
    word=w,
    currentWord="",
    currentTurn=0,
    currentPos=0,
    tiles=generateTiles w 6,
    inputInterface=generateKeyboardList (-200, -185) keyboard,
    gameResult=PLAY
  }

{-
  The function receives the pressed key, its coordinates and
  the current state of the playing field and using
  previously described functions updates or does not change
  playing field state 
-}
handleInput :: Event -> GamePane -> GamePane
handleInput (EventKey (SpecialKey KeyF5) Up _ _) GamePane {wordList=wl, word=w}
    = initialGamePane wl w
handleInput (EventKey (MouseButton LeftButton) Up _ _)
    w@GamePane {gameResult=WIN} = w
handleInput (EventKey (MouseButton LeftButton) Up _ _)
    w@GamePane {gameResult=DEFEAT} = w
handleInput (EventKey (MouseButton LeftButton) Up _ p)
    w@GamePane {inputInterface=ks}       
      = updateGamePane (keyboardTapToVal $ findTappedKey p ks) w
handleInput _ w = w

{-
  The function receives the pressed key (or Nothing if the tap was
  not by the key) and the current game object. This function 
  updates the game object, according the rules, current state of 
  the playing field and the input pressed key
-}
updateGamePane :: Maybe KeyTile -> GamePane -> GamePane
updateGamePane Nothing gp = gp
updateGamePane (Just ENTER)
  gp@GamePane {
    wordList=ws, 
    word=w, 
    currentWord=cw, 
    currentTurn=ct, 
    currentPos=cp, 
    tiles=ts, 
    inputInterface=ks
  }
      | cp == wordLength = changeGameState cw ws   
      | otherwise = gp
        where 
          changeGameState val list 
            | cw == w = GamePane {
                wordList=ws,
                word=w,
                currentWord=cw,
                currentTurn=ct,
                currentPos=0,
                tiles=updateRevealedTiles (ct*wordLength+cp) ts,
                inputInterface=updateInputInterface w cw w ks,
                gameResult=WIN
              }
            | ct == wordLength = GamePane {
                wordList=ws,
                word=w,
                currentWord=cw,
                currentTurn=ct,
                currentPos=0,
                tiles=updateRevealedTiles (ct*wordLength+cp) ts,
                inputInterface=updateInputInterface w cw w ks,
                gameResult=DEFEAT
              }  
            | val `elem` list = GamePane {
                wordList=ws,
                word=w,
                currentWord="",
                currentTurn=ct+1,
                currentPos=0,
                tiles=updateRevealedTiles (ct*wordLength+cp) ts,
                inputInterface=updateInputInterface w cw w ks,
                gameResult=PLAY
              }
            | otherwise = GamePane {
                wordList=ws,
                word=w,
                currentWord=cw,
                currentTurn=ct,
                currentPos=cp,
                tiles=ts,
                inputInterface=ks,
                gameResult=GameResult notInListMessage
              }  
updateGamePane (Just DELETE)
  gp@GamePane {
    wordList=ws, 
    word=w, 
    currentWord=cw, 
    currentTurn=ct, 
    currentPos=cp, 
    tiles=ts, 
    inputInterface=ks
  }
      | cp > 0 = GamePane {
          wordList=ws,
          word=w,
          currentWord=init cw,
          currentTurn=ct,
          currentPos=cp-1,
          tiles=deleteGameTile 0 (ct*wordLength+cp-1) ts,
          inputInterface=ks,
          gameResult=PLAY
        }
      | otherwise = gp
updateGamePane (Just (KeyTile newVal))
  gp@GamePane {
    wordList=ws, 
    word=w, 
    currentWord=cw, 
    currentTurn=ct, 
    currentPos=cp, 
    tiles=ts, 
    inputInterface=ks
  }
      | cp < wordLength = GamePane {
          wordList=ws,
          word=w,
          currentWord=cw ++ [newVal],
          currentTurn=ct,
          currentPos=cp+1,
          tiles=updateGameTile 0 (ct*wordLength+cp) newVal ts,
          inputInterface=ks,
          gameResult=PLAY
        }
      | otherwise = gp

{-
  The function receives the intended word (two times) and the word
  entered by the user on the current line, the word is passed twice 
  because colors are calculated in the function buttons, and 
  for this purpose one need to save both the whole word
  and compare it with the entered letter by letter, the function 
  itself returns the colored keyboard 
-}
updateInputInterface :: String -> String -> String -> [KeyboardButton] -> [KeyboardButton]
updateInputInterface [] _ _ l = l
updateInputInterface _ [] _ l = l   
updateInputInterface (w:ws) (cw:cws) ans l 
  | w == cw = 
    updateInputInterface ws cws ans (updateKeyTile cw rightPosColor l)
  | cw `elem` ans = 
    updateInputInterface ws cws ans (updateKeyTile cw rightLetterColor l)
  | otherwise = 
    updateInputInterface ws cws ans (updateKeyTile cw wrongLetterColor l)     

{-
  The function changes the color of a button which value 
  matches with value passed as function argument 
-}
updateKeyTile :: Char -> Color -> [KeyboardButton] -> [KeyboardButton]
updateKeyTile _ _ [] = []
updateKeyTile l c (kb@KeyboardButton {key=k, position=p}:ks) 
  | [l] == k = KeyboardButton {key=k, position=p, col=c}:ks  
  | otherwise = kb:updateKeyTile l c ks

{-
  The function receives a number n and a list of game tiles
  and changes the value of the hidden flag on the first n elements 
-}
updateRevealedTiles :: Int -> [GameTile] -> [GameTile]
updateRevealedTiles n l = let f GameTile{myVal=mv, trueVal=tv} = 
                                GameTile{myVal=mv, trueVal=tv, hidden=False}
                          in map f (take n l) ++ drop n l

{-
  The function is similar to the previous one, 
  but this function appends value entered by the user
-}
updateGameTile :: Int -> Int -> Char -> [GameTile] -> [GameTile]
updateGameTile _ _ _ [] = []
updateGameTile n i newVal (x@GameTile{trueVal=v}:xs) 
  | n == i = (GameTile {myVal=Just newVal, trueVal=v, hidden=True}):xs
  | otherwise = x:updateGameTile (n+1) i newVal xs

{-
  The function gets the start index to iterate,
  the index of the required object and the list of game tiles
  and removes the value that the player entered in this cell.
  The function returns the modified list
-}
deleteGameTile :: Int -> Int -> [GameTile] -> [GameTile]
deleteGameTile _ _ [] = []
deleteGameTile n i (x@GameTile{trueVal=v}:xs) 
  | n == i = (GameTile {myVal=Nothing, trueVal=v, hidden=True}):xs
  | otherwise = x:deleteGameTile (n+1) i xs

{-
  The function gets the intended word and the number of lines, and
  generates a list of game tile objects of size
  LxN where L is the word length and N is the number of lines
-}  
generateTiles :: String -> Int -> [GameTile]
generateTiles _ 0 = []
generateTiles w n = let f x = GameTile {myVal=Nothing, trueVal=x, hidden=True}
                    in map f w ++ generateTiles w (n-1)

{-
  The function receives the starting position of the keyboard 
  generation and the list of string buttons and 
  returns a list of key objects
-}
generateKeyboardList :: (Float, Float) -> [String] -> [KeyboardButton]
generateKeyboardList _ [] = []
generateKeyboardList (x, y) (l:ls)  
  | l == "P" = 
     newElem:generateKeyboardList (-177,y-keyTileHeight-13) ls
  | l == "L" =
     newElem:generateKeyboardList (-200+keyTileWidth/3-2,y-keyTileHeight-13) ls
  | l == enter || l == "M" =
     newElem:generateKeyboardList (x+keyTileWidth+17,y) ls
  | otherwise =
     newElem:generateKeyboardList (x+keyTileWidth+5,y) ls
  where newElem = KeyboardButton{position=(x, y), key=l, col=keyTileColor}   
  
{-
  The function returns possible value of the pressed 
  key based on the string value of the key, and 
  since the findTappedKey function can return 
  an empty list, then this function returns Maybe KeyTile
-}
keyboardTapToVal :: [KeyboardButton] -> Maybe KeyTile
keyboardTapToVal [] = Nothing
keyboardTapToVal (KeyboardButton{key=k}:_) = Just (valToKey k)
                  where valToKey val | val == enter = ENTER
                                     | val == delete = DELETE
                                     | otherwise = KeyTile $ head val

{-
  The function receives the coordinates of the hit point and a 
  list of key objects, which contains the coordinates of the center point. 
  After that this function finds which key this click belongs to
  using function pointTapArea. If the click was on an area that does not belong
  to the keyboard, the function will return an empty list
-} 
findTappedKey :: (Float, Float) -> [KeyboardButton] -> [KeyboardButton]
findTappedKey keyPos = 
  filter (\ KeyboardButton {position = p} -> pointInTapArea keyPos p)

{-
  The function receives the coordinates of the click point and the coordinates 
  of keyboard button center and calculates whether the click point lies within 
  the area of specific button based on the size of this button 
-}                        
pointInTapArea :: (Float, Float) -> (Float, Float) -> Bool
pointInTapArea (x, y) (s1, s2) = 
  x >= (s1 - keyTileWidth/2) && x <= (s1 + keyTileWidth/2)
  && y >= (s2 - keyTileHeight) && y <= (s2 + keyTileHeight)  