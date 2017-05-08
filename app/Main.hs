module Main where

import Graphics.Proc

main :: IO ()
main = runProc $ def
    { procSetup  = setup
    , procDraw   = draw
    , procUpdate = update
    }

-- constants

rad = 55
sizes  = (300, 300)
center = 0.5 *^ sizes

-- standard functions

setup :: Pio Float
setup = do
    size sizes
    return 0

draw :: Float -> Draw
draw t = do
    background (grey 255)
    drawSun
    drawPlanet t

update :: Float -> Pio Float
update t = return (t + 0.0025)

-- drawing

drawSun = do
    fill (grey 0)
    ellipse center 30

drawPlanet t = do
    fill (grey 145)
    ellipse p 12
    where    
        p = center + rad *^ (cos t, sin t)    

mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2)= (f a1, f a2)

