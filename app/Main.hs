module Main(main) where

import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (800, 800) (10, 10)

background :: Color
background = white

rotateRightInPlace :: Float -> Picture -> Picture
rotateRightInPlace height pic = translate 0 height $ rotate 90 pic
rotateLeftInPlace :: Float -> Picture -> Picture
rotateLeftInPlace width pic = translate width 0 $ rotate (-90) pic

phc :: Int -> Picture
phc 1 = color black $ line [(0,0), (0,1), (1,1), (1,0)]
phc n = pictures [
    translate 0 0 $ rotateRightInPlace (lastSize-1) lastPic,
    color black $ line [(0, lastSize-1), (0, lastSize)],
    translate 0 lastSize lastPic,
    color black $ line [(lastSize-1,lastSize), (lastSize,lastSize)],
    translate lastSize lastSize lastPic,
    color black $ line [(mySize - 1, lastSize), (mySize - 1, lastSize-1)],
    translate lastSize 0 $ rotateLeftInPlace (lastSize - 1) lastPic]
    where 
        lastOrder = n - 1
        lastSize = 2 ^ lastOrder
        lastPic = phc lastOrder
        mySize = 2 ^ n

phcScaledAndCropped :: Int -> Picture
phcScaledAndCropped order = translate (-395) (-395) $ scale scaleFraction scaleFraction $ phc order
    where
        size = 2 ^ order
        scaleFraction = 800 / size

drawing :: Picture
drawing = phcScaledAndCropped 6

main :: IO ()
main = display window background drawing