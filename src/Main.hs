module Main where

import GHC.Float

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Pos = (Float, Float)

type Vel = (Float, Float)

data Ball = Ball Pos Vel

type Paddle = Float

data GameState = GameState {
              ball :: Ball
            , paddle1 :: Paddle
            , paddle2 :: Paddle
            }


ballPos :: GameState -> Pos
ballPos state = let (Ball pos _) = ball state in pos

ballVel :: GameState -> Vel
ballVel state = let (Ball _ vel) = ball state in vel

paddle1Pos :: GameState -> Float
paddle1Pos = paddle1

paddle2Pos :: GameState -> Float
paddle2Pos = paddle2


fps = 60

height = 480
width = height * (4/3)

background = black

displayConfig = InWindow "Pong" (round width :: Int, round height :: Int) (10, 10)

ballRadius = 20

paddleHeight = height/4
paddleWidth = 20


initBall = Ball (height/2, width/2) (width/1.5, height/1.5)

initPaddle = height/2

initState :: GameState
initState = GameState initBall initPaddle initPaddle


paddleBounds :: GameState -> GameState
paddleBounds state
  | pos1 < 0                     = state { paddle1 = 0 }
  | pos1 + paddleHeight > height = state { paddle1 = height - paddleHeight }
  | pos2 < 0                     = state { paddle2 = 0}
  | pos2 + paddleHeight > height = state { paddle2 = height - paddleHeight }
      where
        pos1 = paddle1Pos state
        pos2 = paddle2Pos state
 
movePaddle :: Event -> GameState -> GameState
movePaddle (EventKey key _ _ _) state = paddleBounds $
    case key of
      (Char 'w') -> state { paddle1 = (paddle1Pos state) - 5 }
      (Char 's') -> state { paddle1 = (paddle1Pos state) + 5 }
      (SpecialKey KeyUp) -> state { paddle2 = (paddle2Pos state) - 5 }
      (SpecialKey KeyDown) -> state { paddle2 = (paddle2Pos state) + 5 }

movePaddle _ state = state

ballBounds :: GameState -> GameState
ballBounds state
  | x - ballRadius < 0      = initState
  | x + ballRadius > width  = initState
  | y - ballRadius < 0      = state { ball = Ball (x, ballRadius) (vx,(-vy)) }
  | y + ballRadius > height = state { ball = Ball (x, height - ballRadius) (vx,(-vy)) }
      where
        (x,y)   = ballPos state
        (vx,vy) = ballVel state

updateState :: Float -> GameState -> GameState
updateState time state = ballBounds $ state { ball = ball' }
  where
    (x,y)   = ballPos state
    (vx,vy) = ballVel state

    dx = vx*time
    dy = vy*time

    x' = x + dx
    y' = y + dy

    ball' = Ball (x',y') (vx,vy)


render :: GameState -> Picture
render state = 
  pictures [ball, paddle 20 $ paddle1Pos state, paddle (width - paddleWidth - 20) $ paddle2Pos state]
    where
      ball = (uncurry translate) (ballPos state) $ color white $ circleSolid ballRadius
      paddle x y = (uncurry translate) (x,y) $ color white $ rectangleSolid paddleWidth paddleHeight


main :: IO ()
main = play displayConfig black fps initState render movePaddle updateState
