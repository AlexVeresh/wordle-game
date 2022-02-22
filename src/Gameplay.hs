module Gameplay where

import Data
import Constants  
import Graphics.Gloss.Interface.IO.Game

{-
  Функция получает список возможных слов из файла,
  задуманное слово и генерирует объект начального
  игрового поля  
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
  Функция получает нажатую клавишу, ее координаты и 
  текущие состояние игрового поля и с помощью
  раннее описанных функций обновляет или не меняет
  состояние игрового поля  
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
  Функция получает нажатую клавишу (или Nothing, если нажатие было
  не по клавише) и текущий объект игры, в зависимости от текущего
  состояния игрового поля и нажатой клавиши обновляет объект игры,
  в соответсвии с правилами  
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
      | cp == 5 = changeGameState cw ws   
      | otherwise = gp
        where 
          changeGameState val list 
            | cw == w = GamePane {
                wordList=ws,
                word=w,
                currentWord=cw,
                currentTurn=ct,
                currentPos=0,
                tiles=updateRevealedTiles (ct*5+cp) ts,
                inputInterface=updateInputInterface w cw w ks,
                gameResult=WIN
              }
            | ct == 5 = GamePane {
                wordList=ws,
                word=w,
                currentWord=cw,
                currentTurn=ct,
                currentPos=0,
                tiles=updateRevealedTiles (ct*5+cp) ts,
                inputInterface=updateInputInterface w cw w ks,
                gameResult=DEFEAT
              }  
            | val `elem` list = GamePane {
                wordList=ws,
                word=w,
                currentWord="",
                currentTurn=ct+1,
                currentPos=0,
                tiles=updateRevealedTiles (ct*5+cp) ts,
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
          tiles=deleteGameTile 0 (ct*5+cp-1) ts,
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
      | cp < 5 = GamePane {
          wordList=ws,
          word=w,
          currentWord=cw ++ [newVal],
          currentTurn=ct,
          currentPos=cp+1,
          tiles=updateGameTile 0 (ct*5+cp) newVal ts,
          inputInterface=ks,
          gameResult=PLAY
        }
      | otherwise = gp

{-
  Функция получает задуманное слово (два раза) и слово
  которое ввел пользователь с текущей строке, два раза
  слово передается потому что в функции вычисляются цвета
  кнопок, а для этого нужно сохранить как все слово целиком
  так и сравнивать его с введенным побуквенно, сама функция 
  возвращает раскарешнную клавиатуру 
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
  Функция меняет цвет кнопки, значение которой совпадает
  с значением переданным как аргумент функции 
-}
updateKeyTile :: Char -> Color -> [KeyboardButton] -> [KeyboardButton]
updateKeyTile _ _ [] = []
updateKeyTile l c (kb@KeyboardButton {key=k, position=p}:ks) 
  | [l] == k = KeyboardButton {key=k, position=p, col=c}:ks  
  | otherwise = kb:updateKeyTile l c ks

{-
  Функция получает число n и список игровых тайлов
  и меняет значние флага hidden у первых n элементов 
-}
updateRevealedTiles :: Int -> [GameTile] -> [GameTile]
updateRevealedTiles n l = let f GameTile{myVal=mv, trueVal=tv} = 
                                GameTile{myVal=mv, trueVal=tv, hidden=False}
                          in map f (take n l) ++ drop n l

{-
  Функция аналогично предыдущей, только функция добавляет 
  введенное юзером значением
-}
updateGameTile :: Int -> Int -> Char -> [GameTile] -> [GameTile]
updateGameTile _ _ _ [] = []
updateGameTile n i newVal (x@GameTile{trueVal=v}:xs) 
  | n == i = (GameTile {myVal=Just newVal, trueVal=v, hidden=True}):xs
  | otherwise = x:updateGameTile (n+1) i newVal xs

{-
  Функция получает начальный индекс для итерации, 
  индекс требуемого объекта и список игровых тайлов
  и удаляет значние, которое ввел игрок в эту клетку,
  возвращает измененный список
-}
deleteGameTile :: Int -> Int -> [GameTile] -> [GameTile]
deleteGameTile _ _ [] = []
deleteGameTile n i (x@GameTile{trueVal=v}:xs) 
  | n == i = (GameTile {myVal=Nothing, trueVal=v, hidden=True}):xs
  | otherwise = x:deleteGameTile (n+1) i xs

{-
  Функция получает задуманное слово и количество строк, и
  генерирует список объектов игровых тайлов размером
  LxN где L-длина слова, а N-количество строк
-}  
generateTiles :: String -> Int -> [GameTile]
generateTiles _ 0 = []
generateTiles w n = let f x = GameTile {myVal=Nothing, trueVal=x, hidden=True}
                    in map f w ++ generateTiles w (n-1)

{-
  Функция получает начальную позицию генерации клавиатуры и строковый
  список кнопок и возвращает список объектов клавиш
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
  Функция по строковому значению клавиши возвращает 
  возможное значение нажатой клавиши с помощью объекта
  KeyTile, причем так как функция findTappedKey
  может вернуть и пустой список, то эта функция возвращает
  Maybe KeyTile
-}
keyboardTapToVal :: [KeyboardButton] -> Maybe KeyTile
keyboardTapToVal [] = Nothing
keyboardTapToVal (KeyboardButton{key=k}:_) = Just (valToKey k)
                  where valToKey val | val == enter = ENTER
                                     | val == delete = DELETE
                                     | otherwise = KeyTile $ head val

{-
  Функция получает координаты точки нажатия и список объектов клавиш,
  каждая из которых содержит координаты точки центра,и с помощью 
  функции pointTapArea находит к какой именно клавише относится 
  это нажатие, если нажатие было на область, которая не относится 
  к клавиатуре то функция вернет пустой список
-} 
findTappedKey :: (Float, Float) -> [KeyboardButton] -> [KeyboardButton]
findTappedKey keyPos = 
  filter (\ KeyboardButton {position = p} -> pointInTapArea keyPos p)

{-
  Функция получает координаты точки нажатия и координаты центра
  кнопки клавиатуры и исходя из размеров кнопки вычисляет 
  лежит ли точка нажатия в области опредленной кнопки 
-}                        
pointInTapArea :: (Float, Float) -> (Float, Float) -> Bool
pointInTapArea (x, y) (s1, s2) = 
  x >= (s1 - keyTileWidth/2) && x <= (s1 + keyTileWidth/2)
  && y >= (s2 - keyTileHeight) && y <= (s2 + keyTileHeight)  