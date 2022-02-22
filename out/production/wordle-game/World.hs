module World where

import Graphics.Gloss
import Constants
import Data

{-
  Функция получает задуманное слово и объект, описывающий всю
  игру (визуальную часть и состояние игры) и с помощью всех
  нижеописанных функций рисует весь интерфейс 
-}
drawGamePane :: String -> GamePane -> Picture
drawGamePane w GamePane {tiles=ts, inputInterface=ks, gameResult=gr} = 
  pictures [
     drawControlPane, 
     drawGameTitle, 
     drawGameTiles w ts, 
     drawInputPane ks, 
     drawGameResult w gr
   ]

{-
  Функция получает список объектов, которые реализуют логику описания
  кнопок клавиатуры игры и рисует сам интерфейс клавиатуры 
-}
drawInputPane :: [KeyboardButton] -> Picture
drawInputPane x = let f KeyboardButton{position=(p1, p2), key=l, col=c} = 
                        translate p1 p2 $ makeKeyboardTile c l
                  in pictures $ map f x 

{-
  Функция рисует информацию сбоку   
-}   
drawControlPane :: Picture
drawControlPane = translate (-720) 340 $ scale 0.15 0.15 $ text restartMessage

{-
  Функция рисует название игры сверху   
-}
drawGameTitle :: Picture
drawGameTitle = translate (-42) 338 $ scale 0.28 0.28 $ text title

{-
  Функция получает на вход задуманное слово и объект
  GameResult и в зависимости текущего игрового состояния
  выводит сообщение между игровым полем и полем клавиатуры   
-}
drawGameResult :: String -> GameResult -> Picture
drawGameResult _ PLAY = pictures[]
drawGameResult _ WIN = 
  translate (-185) (-135) $ scale 0.23 0.23 $ text winMessage
drawGameResult w DEFEAT = 
  translate (-185) (-135) $ scale 0.23 0.23 $ text (defeatMessage ++ w)
drawGameResult _ (GameResult val) = 
  translate (-185) (-135) $ scale 0.23 0.23 $ text val

{-
  Функция генерирует всё игровое поле, 
  отрисовывая ее в определнном месте  
-}
drawGameTiles :: String -> [GameTile] -> Picture
drawGameTiles w x = pictures $ generateGameTilesPane w 6 (-115, 305) x

{-
  Функция с помощью генерации строк, генерирует всё поле, вызывая
  функцию строки с разным параметром начальной точки отрисовки  
-}
generateGameTilesPane :: String -> Int -> (Float, Float) -> [GameTile] -> [Picture]
generateGameTilesPane _ 0 _ _ = []
generateGameTilesPane w n (x, y) l = generateGameTilesRow w (x, y) (take 5 l) ++
  generateGameTilesPane w (n-1) (x, y-gameTileSize-5) (drop 5 l)

{-
  Функция принимает слово на вход, начальную точку и список объектов, 
  которые используются для основного геймплея(они содержат правильную букву,
  ту букву, которую поставил игрок и значение hidden, которое говорит о том,
  надо ли отображать букву игрока сейчас или нет). Функция рисует картинки
  игровых тайлов, цвет и текст которых зависит от параметров переданного объекта,
  функция возвращает имеено одну строку игрового поля  
-}
generateGameTilesRow :: String -> (Float, Float) -> [GameTile] -> [Picture]
generateGameTilesRow _ _ [] = []
generateGameTilesRow w (x, y) (GameTile {myVal=mv, trueVal=tv, hidden=h}:ls) = 
  translate x y (makeGameTile mv tv w h):
  generateGameTilesRow w (x+gameTileSize+5, y) ls

{-
  Аналогична функции makeKeyboardTile, но тут
  присутствует логика определения цвета, которая
  опирается на правила игры 
-}                  
makeGameTile :: Maybe Char -> Char -> String -> Bool -> Picture
makeGameTile Nothing _ _ _ = pictures [line $ roundedRectPath gameTileSize gameTileSize 0]
makeGameTile (Just v1) v2 w h 
  | h = 
    pictures [line $ roundedRectPath gameTileSize gameTileSize 0, tileText [v1]]
  | v1 == v2 = 
    pictures [color rightPosColor gameTile, color white $ tileText [v1]]
  | v1 `elem` w = 
    pictures [color rightLetterColor gameTile, color white $ tileText [v1]]
  | otherwise = 
    pictures [color wrongLetterColor gameTile, color white $ tileText [v1]]
  where gameTile = polygon $ roundedRectPath gameTileSize gameTileSize 0
        tileText val = translate (-10) (-38) $ scale 0.18 0.18 $ text val
                         
{-
  Функция принимает на вход цвет и текст кнопки, 
  рисует картинку кнопки заданного цвета и с текстом 
  внутри, используя функцию для отрисовки прямоугольника
  и функцию polygon которая объекты типа Path красит внутри
  контура и возвращает цветную картинку Picture.
-}
makeKeyboardTile :: Color -> String -> Picture
makeKeyboardTile c val 
  | val == enter = 
    pictures [bigBtn, translate (-20) (-25) $ scale 0.1 0.1 colorText]
  | val == delete =
    pictures [bigBtn, translate (-23) (-25) $ scale 0.1 0.1 colorText]
  | otherwise = 
    pictures [smallBtn, translate (-10) (-10) $ scale 0.1 0.1 colorText]
    where smallBtn = color c $ polygon $ 
                      roundedRectPath keyTileWidth keyTileHeight keyTileRadius
          bigBtn = color c $ polygon $ 
                      roundedRectPath (keyTileWidth*1.56) keyTileHeight keyTileRadius           
          colorText | c == keyTileColor = text val
                    | otherwise = color white $ text val 

{-
  Функция принимает на вход длину, ширину и радиус углов
  и рисует прямоугольник с скругленными углами (его контур)
-}
roundedRectPath :: Float -> Float -> Float -> Path
roundedRectPath w h r = [(-w/2, -h+r)] ++ reverse (arcPath (-w/2+r, 0) (-r) r) 
  ++ [(0, r)] ++ arcPath (w/2-r, 0) r r ++ [(w/2, -h+r)]
  ++ reverse (arcPath (w/2-r, -h+r) r (-r)) ++ [(-w/2+r, -h)]
  ++ arcPath (-w/2+r, -h+r) (-r) (-r)

{-
  Функция принимает на вход точку, а также два расстояния 
  по оси x и по оси y (их можно считать как радиус со знаком),
  потому их и два, потому что знаки разные могут быть.
  Сама функция рисует арку(1/4 круга в точке с заданным радусом) 
-}
arcPath :: (Float, Float) -> Float -> Float -> Path
arcPath (x, y) r1 r2 = 
  let toPoint x0 y0 d1 d2 a = (x0 + d1*cos a, y0 + d2*sin a)  
  in map (toPoint x y r1 r2) (map (\an -> (an*pi)/180) (reverse [0..90]))                    
                    
                    