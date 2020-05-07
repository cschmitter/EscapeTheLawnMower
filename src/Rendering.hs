module Rendering where

import World

import Graphics.Gloss

render :: World -> Picture
render w = Pictures $ (terrainPictures w) ++ (baddiePictures w) ++ [playerPicture w]

terrainPictures :: World -> [Picture]
terrainPictures = map drawBlock . terrain
  where drawBlock b = case blockType b of 
                        Dirt -> drawDirt b
                        _otherwise -> Blank

drawDirt :: Block -> Picture
drawDirt b = Color colorDirt
             $ Translate x y
             $ rectangleSolid w h
  where (x, y) = position b
        (w, h) = size b

baddiePictures :: World -> [Picture]
baddiePictures _ = []

playerPicture :: World -> Picture
playerPicture wd = Color colorPlayer
                $ Translate x y
                $ rectangleSolid w h
  where p = player wd
        (x, y) = position $ playerBlock p
        (w, h) = size $ playerBlock p

-- colorDirt 733E13
-- colorPlayer 449633
-- colorSky 7BB3B8
  
colorDirt :: Color
colorDirt = makeColorI 115 62 19 255

colorPlayer :: Color
colorPlayer = makeColorI 68 150 51 255
  
colorSky :: Color
colorSky = makeColorI 123 179 184 255
