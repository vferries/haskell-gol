import Haste
import Haste.Graphics.Canvas

type Pos = (Int, Int)

type World = [Pos]

nextDay :: World -> World
nextDay world = filter alive [(x, y) | x <- [0..100], y <- [0..100]]
    where alive pos = case countNeighbours world pos of
                           3 -> True
                           2 -> pos `elem` world
                           _ -> False

neighbours :: Pos -> [Pos]
neighbours (x, y) = [(x+a, y+b) | a <- [-1..1], b <- [-1..1], (a, b) /= (0, 0)]

countNeighbours :: World -> Pos -> Int
countNeighbours world pos = length (filter present (neighbours pos))
    where present pos = pos `elem` world

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
    mapM filledSquare world
  setTimer (Once 5000) $ animate can (nextDay world)
  return ()

myWorld :: World
myWorld = [(49, 49), (49, 50), (49, 51), (50, 51), (51, 51), (51, 50), (51, 49)]
