import Haste
import Haste.Graphics.Canvas

data State = Alive | Dead
    deriving Eq

type Pos = (Int, Int)

type World = Pos -> State

nextDay :: World -> World
nextDay world pos = case countNeighbours world pos of
    3 -> Alive
    2 -> world pos
    _ -> Dead

neighbours :: Pos -> [Pos]
neighbours (x, y) = [(x+a, y+b) | a <- [-1..1], b <- [-1..1], (a, b) /= (0, 0)]

countNeighbours :: World -> Pos -> Int
countNeighbours world pos = length (filter alive (neighbours pos))
    where alive pos = world pos == Alive

squareShape :: (Double, Double) -> Shape ()
squareShape (x, y) = do
  rect (x * 10, y * 10) (x * 10 + 9, y * 10 + 9)

filledSquare :: Pos -> Picture ()
filledSquare (x, y) = fill (squareShape (fromIntegral x, fromIntegral y))

main :: IO ()
main = do
  Just can <- getCanvasById "canvas"
  animate can myWorld

animate :: Canvas -> World -> IO ()
animate can world = do
  render can $ do
    mapM filledSquare (filter (\pos -> world pos == Alive) [(x, y) | x <- [0..2], y <- [0..2]])
  setTimer (Once 1000) $ animate can (nextDay world)
  return ()

myWorld :: World
myWorld (0,1) = Alive
myWorld (1,1) = Alive
myWorld (2,1) = Alive
myWorld _ = Dead
