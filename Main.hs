{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Data.Text (Text)
import Graphics.Blank

main :: IO ()
main = blankCanvas 3000 { events = ["mousemove"] } $ go

type Ball = ((Double, Double), (Double, Double))

type Brick = (Double, Double, Double, Double, Bool, Text, Int)

type Paddle = (Double, Double, Double, Double)

dot :: (Double, Double) -> (Double, Double)-> Double
dot (x,y) (a, b) = x*a + y*b

normal :: (Double, Double) -> (Double, Double)
normal (x, y) = (x / (sqrt (x^2 + y^2)) , y / (sqrt (x^2 + y^2)))


reflection :: (Double, Double) -> (Double, Double) -> (Double, Double)
reflection d@(x, y) n@(nx, ny) = (x - 2*(x*nx + y*ny)*nx,(-1) * abs (y - 2*(x*nx + y*ny)*ny))

showBall :: Ball -> Canvas ()
showBall ((x,y),_) = do
        beginPath()
        globalAlpha 0.5
        fillStyle "red"
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
    
hitPaddle :: Ball -> Paddle -> Double -> Ball
hitPaddle theBall@((x,y), (hVel, vVel)) thePaddle@(xl, xr, yb, yt) speed =
    if ((x + hVel >= xl) && (x + hVel <= xr) && (y + vVel >= yb) && (y + vVel <= yt))
       then if(hVel >= 0)
           then ((x,y), r)
           else ((x,y), r)
    else theBall
    where xpos = (x - xr)
          a = 500 :: Double
          b = 10 :: Double
          yy = (b / a)* sqrt (a^2 - xpos^2)
          n = normal (xpos/a^2, yy/b^2)
          r = reflection (hVel, vVel) n
                    
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
    if ((y + vVel >= height) && not lose)
       then True
       else False
       
showLoseScreen :: (Double, Double) -> Ball -> Bool -> Canvas ()
showLoseScreen (width, height) theBall@((x,y), (hVel, vVel)) lose =
    if ((y + vVel >= height) && lose)
     then do
                clearRect (0,0,width,height)
                lineWidth 1
                strokeStyle "red"
                font "30pt Calibri"
                fillText("You Lose!",10,50)
                fillText("Refresh to play again!",10,150)
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

speedUp :: Paddle -> Double -> (Double, Double) -> (Double, Double)
speedUp (dx,_,_,_) x (hVel,vVel)
    | ((dx - x)/30 + hVel)^2 + vVel^2 <= topSpeed^2 = ((dx -x)/30 + hVel, vVel)
    | otherwise = (signum hVel , vVel)
    where topSpeed = 1.4142135623731 :: Double

go :: DeviceContext -> IO ()
go context = do

     let (w,h) = (width context, height context) :: (Double, Double)
     print (w,h)
     
     let size = 200 :: Double --the size of the paddle
     
     let loop (ball, bricks, paddle, lastPaddleX, lose) = do
         
             send context $ do
                clearCanvas
                showBall ball
                sequence_
                     [ showBrick brks
                     | brks <- bricks
                     ]
                showLoseScreen (w,h) ball lose

                showPaddle paddle

             es <- flush context

             
             let mouseTracker = head ([xx | Just (xx, yy) <- map ePageXY es] ++ [-1])
             print ball
             case mouseTracker of
                -1 -> loop (moveBall $ (hitWall (w, h) $ (hitPaddle (hitAllBricks ball bricks) paddle lastPaddleX)), destroyBricks ball bricks, paddle, lastX paddle, hitBottom (w, h) ball lose)
                x' -> loop (moveBall $ (hitWall (w, h) $ (hitPaddle (hitAllBricks ball bricks) paddle lastPaddleX)), destroyBricks ball bricks, movePaddle paddle x' size, lastX paddle, hitBottom (w, h) ball lose)
             
             
             
     loop (((w/2,h/2),(0.0,1)),(createBricks 10 100 50 0 0 "blue" 1 1) ++ (createBricks 5 100 50 100 100 "purple" 1 1) ++ (createBricks 3 100 50 200 200 "green" 1 1), ((w - size)/2,(w + size)/2,790,800.0),0.0, True)
