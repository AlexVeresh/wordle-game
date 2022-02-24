module World where

import Graphics.Gloss
import Constants
import Data

{-
  The function receives the intended word and an object that describes the whole
  game (the visual part and the state of the game) and with the help of all
  the functions described below draws the entire interface 
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
  The function receives a list of objects that implement the description logic
  of game keyboard buttons and draws the keyboard interface itself
-}
drawInputPane :: [KeyboardButton] -> Picture
drawInputPane x = let f KeyboardButton{position=(p1, p2), key=l, col=c} = 
                        translate p1 p2 $ makeKeyboardTile c l
                  in pictures $ map f x 

{-
  The function draws information on the side  
-}   
drawControlPane :: Picture
drawControlPane = translate (-720) 340 $ scale 0.15 0.15 $ text restartMessage

{-
  The function draws the name of the game on top   
-}
drawGameTitle :: Picture
drawGameTitle = translate (-42) 338 $ scale 0.28 0.28 $ text title

{-
  The function receives the intended word and GameResult object as input
  and outputs a message between the playing field 
  and the keyboard field depending on the current game state  
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
  The function generates the entire playing field,
  drawing it in a certain place
-}
drawGameTiles :: String -> [GameTile] -> Picture
drawGameTiles w x = pictures $ generateGameTilesPane w 6 (-115, 305) x

{-
  The function uses generateGameTilesRow to generate the entire field by calling
  this function with a different rendering start point parameter  
-}
generateGameTilesPane :: String -> Int -> (Float, Float) -> [GameTile] -> [Picture]
generateGameTilesPane _ 0 _ _ = []
generateGameTilesPane w n (x, y) l = generateGameTilesRow w (x, y) (take wordLength l) ++
  generateGameTilesPane w (n-1) (x, y-gameTileSize-tileMargin) (drop wordLength l)

{-
  The function takes a word as input, a starting point and a list of objects,
  which are used for the main gameplay (they contain the correct letter,
  the letter that the player put and the hidden value, which indicates that
  whether to display the player's letter now or not). The function draws
  game tiles pictures, which color and text depends on the parameters 
  of the passed object, the function returns one row of the playing field  
-}
generateGameTilesRow :: String -> (Float, Float) -> [GameTile] -> [Picture]
generateGameTilesRow _ _ [] = []
generateGameTilesRow w (x, y) (GameTile {myVal=mv, trueVal=tv, hidden=h}:ls) = 
  translate x y (makeGameTile mv tv w h):
  generateGameTilesRow w (x+gameTileSize+tileMargin, y) ls

{-
  Similar to the makeKeyboardTile function, but here
  there is a logic for defining the color, which
  based on the rules of the game 
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
  The function takes as input the color and text of the button,
  draws a button image of the specified color and text
  inside, using a function to draw a rectangle
  and the polygon function that takes outlined objects of type Path  
  and returns a colored Picture
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
  The function takes as input the length, width and radius of the corners.
  and draws a rectangle with rounded corners (it's outline)
-}
roundedRectPath :: Float -> Float -> Float -> Path
roundedRectPath w h r = [(-w/2, -h+r)] ++ reverse (arcPath (-w/2+r, 0) (-r) r) 
  ++ [(0, r)] ++ arcPath (w/2-r, 0) r r ++ [(w/2, -h+r)]
  ++ reverse (arcPath (w/2-r, -h+r) r (-r)) ++ [(-w/2+r, -h)]
  ++ arcPath (-w/2+r, -h+r) (-r) (-r)

{-
  The function takes a point as input, as well as two distances.
  along the x-axis and along the y-axis (these can be thought of as a signed radius),
  because there are two of them, because the signs can be different.
  The function itself draws an arc (1/4 of a circle at a point with a given radius) 
-}
arcPath :: (Float, Float) -> Float -> Float -> Path
arcPath (x, y) r1 r2 = 
  let toPoint x0 y0 d1 d2 a = (x0 + d1*cos a, y0 + d2*sin a)  
  in map (toPoint x y r1 r2) (map (\an -> (an*pi)/180) (reverse [0..90]))                    
                    
                    