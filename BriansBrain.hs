import Random ( newStdGen, randomRIO, randomRs, RandomGen )
import Data.List
import Data.Array
import Control.Arrow
import Graphics.UI.SDL hiding (index)
import Control.Monad
import Control.Concurrent
import Control.Parallel.Strategies
import Prelude hiding (flip)

data Cell  = CellOff | CellDying | CellOn deriving (Eq, Ord, Enum, Show)
type World = Array (Int, Int) Cell
type Peers = (Cell, Int)

cellSize = 8
border   = 1

stepCell :: Peers -> Cell
stepCell (CellOff,   2) = CellOn
stepCell (CellOff,   _) = CellOff
stepCell (CellDying, _) = CellOff
stepCell (CellOn,    _) = CellDying

stepWorld :: World -> World
stepWorld world = stepCell `fmap` peersArray world

peersArray :: World -> Array (Int, Int) Peers
peersArray world = getPeers world `fmap` indexArray x y
  where ((1,1),(x,y)) = bounds world

indexArray :: Int -> Int -> Array (Int, Int) (Int, Int)
indexArray x y = array ((1,1),(x,y)) [((x,y),(x,y)) | x <- [1..x], y <- [1..y]]

getPeers :: World -> (Int, Int) -> Peers
getPeers world pos@(x,y) = (world ! pos, living neighbors)
  where neighbors = do
            x <- [x-1 .. x+1]
            y <- [y-1 .. y+1]
            return $ world ! (clip (minX, maxX) x, clip (minY, maxY) y)
        living = length . filter (== CellOn)
        ((minX,minY),(maxX,maxY)) = bounds world

clip :: (Int, Int) -> Int -> Int
clip bounds@(min, max) val
  | val < min = clip bounds $ val + max - min
  | val > max = clip bounds $ val + min - max
  | otherwise = val

randWorld :: RandomGen r => Int -> Int -> r -> World
randWorld x y = array ((1,1),(x,y)) . zip indices . map toEnum . randomRs (0,2)
  where indices = [(x,y) | x <- [1..x], y <- [1..y]]

main = do
    world <- randWorld sizeX sizeY `fmap` newStdGen
    withInit [InitVideo] $ do
        setCaption title title
        surface <- setVideoMode (sizeX*cellSize) (sizeY*cellSize) 24 [DoubleBuf]
        forM_ (iterate stepWorld world) $ \world -> do
            sequence $ do
                x <- [1..sizeX]
                y <- [1..sizeY]
                return $ drawCell surface x y $ world ! (x,y)
            flip surface
  where title = "Brian's Purely Functional Brain"
        (sizeX, sizeY) = (90, 90)

drawCell s x y cell = fillRect s (Just rect) $ color cell
  where color CellOn    = Pixel 0x00FFFFFF
        color CellDying = Pixel 0x00888888
        color CellOff   = Pixel 0x00000000
        rect = Rect sx sy w h
        (sx, sy) = ((x - 1) * cellSize, (y - 1) * cellSize)
        (w, h)   = (cellSize - border, cellSize - border)
