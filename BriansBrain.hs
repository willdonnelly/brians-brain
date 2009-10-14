import Random ( newStdGen, randomRIO, randomRs, RandomGen )
import Data.List
import Control.Arrow
import Graphics.UI.SDL hiding (index)
import Control.Monad
import Control.Concurrent
import Prelude hiding (flip)

data Cell  = CellOff | CellDying | CellOn deriving (Eq, Ord, Enum, Show)
data World = World Int Int [[Cell]]
type Peers = (Cell, Int)

cellSize = 8
border   = 1

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
split n = unfoldr (\x -> case x of [] -> Nothing; xs -> Just $ splitAt n xs)

randWorld :: RandomGen r => Int -> Int -> r -> World
randWorld x y = World x y . take y . split x . map toEnum . randomRs (0,2)

main = do
    world <- randWorld sizeX sizeY `fmap` newStdGen
    withInit [InitVideo] $ do
        setCaption title title
        surface <- setVideoMode (sizeX*cellSize) (sizeY*cellSize) 24 [DoubleBuf]
        forM_ (take 10 $ iterate stepWorld world) $ \(World mx my cells) -> do
            event <- pollEvent
            case event of
                Quit -> quit
                _ -> return ()
            sequence $ do
                x <- [1..mx]
                y <- [1..my]
                return $ drawCell surface x y $ cells `index` (x,y)
            flip surface
--            threadDelay 250000
  where title = "Brian's Purely Functional Brain"
        sizeX = 90
        sizeY = 90

drawCell s x y cell = fillRect s (Just rect) $ color cell
  where color CellOn    = Pixel 0x00FFFFFF
        color CellDying = Pixel 0x00888888
        color CellOff   = Pixel 0x00000000
        rect = Rect sx sy w h
        (sx, sy) = ((x - 1) * cellSize, (y - 1) * cellSize)
        (w, h)   = (cellSize - border, cellSize - border)
