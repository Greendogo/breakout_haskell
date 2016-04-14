{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Data.Text (Text)
import Graphics.Blank
import qualified Graphics.Blank.Style as Style


main :: IO ()
main = blankCanvas 3000 { events = ["mousemove"] } $ go

type Ball = ((Double, Double), (Double, Double))

type Brick = (Double, Double, Double, Double, Bool, Text, Int)

type Paddle = (Double, Double, Double, Double)

-- gives a dot product
-- dot :: (Double, Double) -> (Double, Double)-> Double
-- dot (x,y) (a, b) = x*a + y*b

-- gives the normal to a surface
-- normal :: (Double, Double) -> (Double, Double)
-- normal (x, y) = (x / (sqrt (x^2 + y^2)) , y / (sqrt (x^2 + y^2)))

-- dealing with reflections over a given normal
-- reflection :: (Double, Double) -> (Double, Double) -> (Double, Double)
-- reflection d@(x, y) n@(nx, ny) = (x - 2*(x*nx + y*ny)*nx,(-1) * abs (y - 2*(x*nx + y*ny)*ny))

showBall :: Ball -> Canvas ()
showBall ((x,y),_) = do
        beginPath()
        globalAlpha 1
        fillStyle "black"
        arc(x, y, 5, 0, pi*2, False)
        closePath()
        fill()

showBrick :: Brick -> Canvas ()
showBrick (xl, xr, yb, yt, True, color, hits) = do
    beginPath()
    globalAlpha 1
    fillStyle color
    rect(xl + 1,yb + 1,xr-xl - 1,yt-yb - 1)
    closePath()
    fill()
showBrick (_,_,_,_,False,_,_) = do
    fill()

showPaddle :: Paddle -> Canvas ()
showPaddle (xl, xr, yb, yt) = do
    beginPath()
    globalAlpha 1
    fillStyle "green"
    rect(xl,yb,xr-xl,yt-yb)
    closePath()
    fill()
    beginPath()
    globalAlpha 1
    fillStyle "blue"
    rect(xl + 0.2*(xr - xl),yb, (xr - xl) - 0.4*(xr - xl),yt-yb)
    closePath()
    fill()

movePaddle :: Paddle -> Double -> Double -> Paddle
movePaddle (_, _, yb, yt) d size= (-size/2 + d, size/2 + d, yb, yt) --D here is the x location of the cursor

moveBall :: Ball -> Ball
moveBall ((x,y) , velocity@(hVel, vVel)) = ((x+hVel,y+vVel) , velocity)

hitBrick :: Ball -> Brick -> Ball
hitBrick theBall@((x,y), (hVel, vVel)) (xl, xr, yb, yt,True,_,_) =
    if ((x + hVel >= xl) && (x + hVel <= xr) && (y + vVel >= yb) && (y + vVel <= yt))
       then if(((x <= xl) && (y <= yb))||((x <= xl)&&(y >= yt))||((x >= xr) && (y <= yb))||((x >= xr) && (y >= yt)))
                then ((x,y), (-hVel, -vVel))
                else if ((x <= xl)||(x >= xr))
                    then ((x,y), (-hVel, vVel))
                    else if ((y >= yb)||(y <= yt))
                    then ((x,y), (hVel, -vVel))
                    else ((x,y), (hVel, vVel))
                    else ((x,y), (hVel, vVel))
hitBrick theBall (_, _, _, _,False,_,_) =
    theBall

hitAllBricks :: Ball -> [Brick] -> Ball
hitAllBricks ball [] = ball
hitAllBricks ball (x:xs) = hitAllBricks (hitBrick ball x) xs

-- this version of hitPaddle makes the ball bounce off of the paddle rectangularly.
hitPaddle :: Ball -> Paddle -> Double -> Ball
hitPaddle theBall@((x,y), (hVel, vVel)) thePaddle@(xl, xr, yb, yt) speed =
    if ((x + hVel >= xl) && (x + hVel <= xr) && (y + vVel >= yb) && (y + vVel <= yt))
       then ((x,y), (hVel, -vVel))
    else theBall -- if not contacting the paddle, the ball just keeps going.

-- This version of hitPaddle attempts to map the surface of an ellipse to the paddle
-- and bounce the ball off of it accordingly.  Currently slow and broken.
-- hitPaddle :: Ball -> Paddle -> Double -> Ball
-- hitPaddle theBall@((x,y), (hVel, vVel)) thePaddle@(xl, xr, yb, yt) speed =
--     if ((x + hVel >= xl) && (x + hVel <= xr) && (y + vVel >= yb) && (y + vVel <= yt))
--        then if(hVel >= 0)
--            then ((x,y), r)
--            else ((x,y), r)
--     else theBall
--     where xpos = (x - xr)
--           a = 500 :: Double
--           b = 10 :: Double
--           yy = (b / a)* sqrt (a^2 - xpos^2)
--           n = normal (xpos/a^2, yy/b^2)
--           r = reflection (hVel, vVel) n

hitWall :: (Double, Double) -> Ball -> Ball
hitWall (width , height) theBall@((x,y), (hVel, vVel)) =
    if ((x + hVel >= width) || (x + hVel <= 0) || (y + vVel >= height) || (y + vVel <= 0))
       then if(((x <= 0) && (y <= 0))||((x <= 0)&&(y >= height))||((x >= width) && (y <= 0))||((x >= width) && (y >= height)))
                then ((x,y), (-hVel, -vVel))
                else if ((x <= 0)||(x >= width))
                    then ((x,y), (-hVel, vVel))
                    else if ((y <= 0)||(y >= height))
                    then ((x,y), (hVel, -vVel))
                    else ((x,y), (hVel, vVel))
                    else ((x,y), (hVel, vVel))

hitBottom :: (Double, Double) -> Ball -> Bool -> Bool
hitBottom (width , height) theBall@((x,y), (hVel, vVel)) lose = do
    if (not lose)
      then if ((y + vVel >= height))
           then True
           else False
      else True

showLoseScreen :: (Double, Double, Double, Double) -> Ball -> Bool -> Canvas ()
showLoseScreen (xd, yd, width, height) theBall@((x,y), (hVel, vVel)) lose =
    if ((y + vVel >= height) || lose)
     then do
                clearRect (xd, yd, width, height)
                lineWidth 1
                font "30pt Calibri"
		textAlign "center"
	        fillStyle "red"
                fillText("You Lose!", width/2, height/2)
                fillText("Refresh to play again!", width/2, height/2 + 30)
     else do
            beginPath()
            globalAlpha 1
            fillStyle "green"
            rect(10,10,20,20)

destroyBrick :: Ball -> Brick -> Brick
destroyBrick ((x,y), (hVel, vVel)) (xl, xr, yb, yt, True, color, hits) =
    if ((x + hVel >= xl) && (x + hVel <= xr) && (y + vVel >= yb) && (y + vVel <= yt))
       then if(hits - 1 == 0)
                then (xl, xr, yb, yt, False, color, hits - 1)
                else (xl, xr, yb, yt, False, color, hits - 1)
       else (xl, xr, yb, yt, True, color, hits)
destroyBrick _ theBrick = theBrick

destroyBricks :: Ball -> [Brick] -> [Brick]
destroyBricks ball bricks = [destroyBrick ball w | w <- bricks]

createBricks :: Double -> Double -> Double -> Double -> Double -> Text -> Int -> Int -> [Brick]
createBricks count xSize ySize yRow startCol color hits pattern
    | pattern == 1 = [ (xSize*w + startCol, (xSize)*(w + 1) + startCol, ySize + yRow, (ySize)*2.0 + yRow, True, color, hits) | w <- [1..count]]

lastX :: Paddle -> Double
lastX (x,_,_,_) = x

-- used to put a speed boost on the ball under certain curcumstances.  Currently broken and slow.
-- speedUp :: Paddle -> Double -> (Double, Double) -> (Double, Double)
-- speedUp (dx,_,_,_) x (hVel,vVel)
--     | ((dx - x)/30 + hVel)^2 + vVel^2 <= topSpeed^2 = ((dx -x)/30 + hVel, vVel)
--     | otherwise = (signum hVel , vVel)
--     where topSpeed = 1.4142135623731 :: Double


showRect :: (Double, Double, Double, Double, Text) -> Canvas ()
showRect (xd, yd, w, h, color) = do
    beginPath()
    globalAlpha 1
    fillStyle color
    rect(xd,yd,w,h)
    closePath()
    fill()

showBack :: (Double, Double, Double, Double) -> Canvas ()
showBack (xd, yd, w, h) = do
	beginPath()
	globalAlpha 1
        grd <- createRadialGradient (238, 50, 10, 238, 50, 300)
        -- light blue
        grd # addColorStop(0, "#8ED6FF")
        -- dark blue
        grd # addColorStop(1, "#004CB3")
        Style.fillStyle grd;
	rect(xd, yd, w, h)
        fill();

go :: DeviceContext -> IO ()
go context = do
     let (xd, yd) = (10, 10)
     let (w,h) = ((width context) - xd, (height context) - yd) :: (Double, Double)
    -- uncomment to print the size of the canvas.
    --  print (w,h)

     let size = 200 :: Double --the size of the paddle

     let loop (ball, bricks, paddle, lastPaddleX, lose) = do

             send context $ do
                clearCanvas
                showRect(0, 0, w + xd,h + yd, "black")
		showBack(xd, yd, w - xd, h - yd)
                showBall ball
                sequence_
                     [ showBrick brks
                     | brks <- bricks
                     ]

                showLoseScreen (xd, yd, w - xd, h - yd) ball lose
                showPaddle paddle
             threadDelay (1 * 1000)
             es <- flush context


             let mouseTracker = head ([xx | Just (xx, yy) <- map ePageXY es] ++ [-1])
            -- uncomment to print the ball location and velocity.
            --  print ball
             case mouseTracker of
                -1 -> loop (moveBall $ (hitWall (w, h) $ (hitPaddle (hitAllBricks ball bricks) paddle lastPaddleX)), destroyBricks ball bricks, paddle, lastX paddle, hitBottom (w, h) ball lose)
                x' -> loop (moveBall $ (hitWall (w, h) $ (hitPaddle (hitAllBricks ball bricks) paddle lastPaddleX)), destroyBricks ball bricks, movePaddle paddle x' size, lastX paddle, hitBottom (w, h) ball lose)



     loop (((w/2, h - 200),(1,2)),(createBricks 17 100 50 0 0 "blue" 1 1) ++ (createBricks 15 100 50 100 100 "purple" 1 1) ++ (createBricks 13 100 50 200 200 "green" 1 1), ((w - size)/2,(w + size)/2,h - 100 - 10,h - 100),0.0, False)
