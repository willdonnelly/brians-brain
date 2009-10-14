import Random ( newStdGen, randomRIO, randomRs, RandomGen )
import Data.List
import Control.Arrow
import Graphics.HGL
import Control.Monad
import Control.Concurrent

data Cell  = CellOff | CellDying | CellOn deriving (Eq, Ord, Enum, Show)
data World = World Int Int [[Cell]]
type Peers = (Cell, Int)

stepCell :: Peers -> Cell
stepCell (CellOff,   2) = CellOn
stepCell (CellOff,   _) = CellOff
stepCell (CellDying, _) = CellOff
stepCell (CellOn,    _) = CellDying

index cells (x, y) = cells !! (y - 1) !! (x - 1)

getPeers :: World -> (Int, Int) -> Peers
getPeers (World mx my cells) (x, y) = (index cells (x,y), living indices)
  where living   = length . filter (== CellOn) . map (index cells)
        indices  = [wrap (x+a) (y+b) | b <- [-1..1], a <- [-1..1]]
        wrap x y = (wrap' mx x, wrap' my y)
          where wrap' max val | val > max  = val - max
                              | val < 1    = val + max
                              | otherwise  = val

stepWorld :: World -> World
stepWorld world@(World mx my _) = World mx my $ split mx cells
  where cells = [step (x,y) | y <- [1..my], x <- [1..mx]]
        step  = stepCell . getPeers world

-- | A helper function that the standard Prelude seems
--   to lack.
split :: Int -> [a] -> [[a]]
split n = unfoldr splitNext
  where splitNext [] = Nothing
        splitNext xs = Just $ splitAt n xs

printWorld (World mx my cells) = unlines . map showRow $ cells
  where showRow = concatMap showCell
        showCell CellOn    = "# "
        showCell CellDying = "+ "
        showCell CellOff   = ". "

randWorld :: RandomGen r => Int -> Int -> r -> World
randWorld x y = World x y . take y . split x . map toEnum . randomRs (0,2)

main = do
    world <- randWorld sizeX sizeY `fmap` newStdGen
    let worlds = iterate stepWorld $ world
    runWindow title (sizeX*cellSize, sizeY*cellSize) $ \window -> do
        forM_ worlds $ \world -> do
            drawWorld window world
            threadDelay 250000
  where title = "Brian's Purely Functional Brain"
        sizeX = 90
        sizeY = 90

drawWorld w (World mx my cells) = drawInWindow w $ do
    forM_ ixs $ \(x,y) -> drawCell x y $ index cells (x, y)
  where ixs = [(x,y) | y <- [1..my], x <- [1..mx]]

drawCell x y cell = color cell $ polygon points
  where color CellOn    = withRGB $ RGB 255 255 255
        color CellDying = withRGB $ RGB 127 127 127
        color CellOff   = withRGB $ RGB 0   0   0
        points   = [(sx,sy), (sx+w, sy), (sx+w, sy+h), (sx, sy+h)]
        (sx, sy) = ((x - 1) * cellSize, (y - 1) * cellSize)
        (w, h)   = (cellSize - border, cellSize - border)

cellSize = 8
border = 1
