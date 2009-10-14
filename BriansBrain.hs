import Data.Array
import System.Random
import Graphics.UI.SDL as SDL

data Cell  = Off | Dying | On deriving (Eq, Enum)
type World = Array (Int, Int) Cell
type Peers = (Cell, Int)

title    = "Brian's Purely Functional Brain"
cellSize = 8
border   = 1
worldX   = 90
worldY   = 90
fillSize = cellSize - border
screenX  = worldX * cellSize
screenY  = worldY * cellSize

stepCell (Off,   2) = On
stepCell (Off,   _) = Off
stepCell (Dying, _) = Off
stepCell (On,    _) = Dying

stepWorld w    = stepCell `fmap` peersArray w
peersArray w   = getPeers w `fmap` indexArray worldX worldY
indexArray x y = array ((1,1),(x,y)) [((x,y),(x,y)) | x <- [1..x], y <- [1..y]]

getPeers world pos@(x,y) = (world ! pos, living neighbors)
  where neighbors = do
            x <- [x-1 .. x+1]
            y <- [y-1 .. y+1]
            return $ world ! (clip (1, worldX) x, clip (1, worldY) y)
        living = length . filter (== On)

clip bounds@(min, max) val
  | val < min = clip bounds $ val + max - min
  | val > max = clip bounds $ val + min - max
  | otherwise = val

randWorld x y = array ((1,1),(x,y)) . zip indices . map toEnum . randomRs (0,2)
  where indices = [(x,y) | x <- [1..x], y <- [1..y]]

main = do initWorld <- randWorld worldX worldY `fmap` newStdGen
          SDL.init [SDL.InitVideo]
          SDL.setCaption title title
          surface <- SDL.setVideoMode screenX screenY 24 [SDL.DoubleBuf]
          mapM (drawWorld surface) (iterate stepWorld initWorld)

drawWorld surface world = do
    sequence $ do x <- [1..worldX]
                  y <- [1..worldY]
                  return $ drawCell surface x y $ world ! (x,y)
    SDL.flip surface

drawCell s x y cell = SDL.fillRect s (Just rect) $ color cell
  where rect    = SDL.Rect (scale x) (scale y) fillSize fillSize
        scale x = (x - 1) * cellSize
        color On    = SDL.Pixel 0x00FFFFFF
        color Dying = SDL.Pixel 0x00888888
        color Off   = SDL.Pixel 0x00000000
