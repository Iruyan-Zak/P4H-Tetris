module Main where

import Graphics.Proc

main :: IO ()
main = runProc $ def
    { procSetup  = setup
    , procDraw   = draw
    , procUpdate = update
    }

-- constants

cells = (32, 24)
renderScale = 25
sizes  = renderScale *^ cells
center = 0.5 *^ sizes

boardCorner = (2, 2)
boardSize = (8,20)

type Width = Int
type Height = Int

data Block = None | Red | Blue | Yellow | Green | Perple | Orenge | Silver
    deriving (Eq, Enum, Bounded)

getColor :: Block -> Col
getColor block
  = [ grey 0
    , rgb 240 120 120
    , rgb 120 120 240
    , rgb 240 240 0
    , rgb 120 240 120
    , rgb 240 0 240
    , rgb 240 120 0
    , grey 180
    ] !! fromEnum block

data Board = Board [[Block]]

initialBoard :: (Width, Height) -> Board
initialBoard (w, h) = Board $ replicate w $ replicate h None

-- standard functions

setup :: Pio Board
setup = do
    size sizes
    frameRate 4
    let (Board b) = initialBoard . mapTuple2 round $ boardSize
    return . Board . snd . foldl (\(block, columns) column ->
        (succOrMinBounds block, (block :(tail  column)):columns)) (None, []) $ b

draw :: Board -> Draw
draw (Board board) = do
    background (grey 255)
    drawBoard board

update :: Board -> Pio Board
update = return . changeColor

-- drawing

drawBoard :: [[Block]] -> Draw
drawBoard board = do
    fill (grey 255)
    let d = rect (renderScale *^ boardCorner) (renderScale *^ boardSize)
    snd $ foldl (\(x, draw) column ->
            let (_, draw'') =
                    foldl (\(y, draw') block ->
                        (y + 1, draw' >> drawBlock (x, y) (getColor block))
                    ) (snd boardCorner, draw) column
            in (x + 1, draw'')
        ) (fst boardCorner, d) board

drawBlock :: P2 -> Col -> Draw
drawBlock corner color = do
    fill color
    rect (renderScale *^ corner) $ join (,) renderScale

changeColor :: Board -> Board
changeColor (Board board) = Board $ map changeColumn board
    where
        changeColumn (l:rs) =
            (succOrMinBounds l :) . reverse .
                snd . foldr (\block (below, column) ->
                    (block, below : column)) (l, []) $ reverse rs

succOrMaxBounds :: (Eq a, Enum a, Bounded a) => a -> a
succOrMaxBounds a
    | a == maxBound = maxBound
    | otherwise = succ a

succOrMinBounds :: (Eq a, Enum a, Bounded a) => a -> a
succOrMinBounds a
    | a == maxBound = minBound
    | otherwise = succ a

mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)

