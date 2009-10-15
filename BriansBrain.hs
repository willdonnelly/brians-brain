import Data.Array             -- Used to store the world state for processing
import System.Random          -- Used to generate the initial random world
import Control.Monad          -- Used for some fancy looping constructs
import Control.Concurrent     -- Used to fork the quit event handler
import Graphics.UI.SDL as SDL -- Used to draw the pretty pictures
import Control.Parallel.Strategies

data Cell  = Off | Dying | On deriving (Eq, Enum)

worldX   = 90                -- The horizontal size of the world
worldY   = 90                -- The vertical size of the world
cellSize = 8                 -- The overall size of a cell
border   = 1                 -- The border width between cells
screenX  = worldX * cellSize -- The horizontal size of the world, in pixels
screenY  = worldY * cellSize -- The vertical size of the world, in pixels
fillSize = cellSize - border -- The size of the filled area in each cell

stepCell (Off,   2) = On     -- If a dead cell has 2 live neighbors, turn on
stepCell (Off,   _) = Off    --   Otherwise, just stay turned off
stepCell (Dying, _) = Off    -- Dying cells always turn off
stepCell (On,    _) = Dying  -- Live cells always start to die

indexArray x y = listArray ((1,1),(x,y)) [(a,b) | a <- [1..x], b <- [1..y]]
stepWorld w    = newWorld `using` parArr rwhnf
  where newWorld = fmap (stepCell . getPeers w) $ indexArray worldX worldY

getPeers world (x,y) = (world ! (x,y), length . filter (== On) $ neighbors)
  where neighbors    = [getCell x y | x <- [x-1 .. x+1], y <- [y-1 .. y+1]]
        getCell x y  = world ! (clip worldX x, clip worldY y)
        clip max val | val <  1  = clip max $ val + max - 1
                     | val > max = clip max $ val - max + 1
                     | otherwise = val

main = do rng <- newStdGen
          SDL.init [SDL.InitVideo]
          SDL.setCaption "Brian's Purely Functional Brain" "Brian's Brain"
          surface <- SDL.setVideoMode screenX screenY 24 [SDL.DoubleBuf]
          forkIO . forever $ waitEvent >>= \e -> when (e == Quit) quit
          mapM (drawWorld surface) (iterate stepWorld $ world rng)
  where world = listArray ((1,1),(worldX,worldY)) . map toEnum . randomRs (0,2)

drawWorld s w = do sequence [draw x y | x <- [1..worldX], y <- [1..worldY]]
                   SDL.flip s
  where draw x y = SDL.fillRect s (Just rect) . color $ w ! (x,y)
          where rect        = SDL.Rect (scale x) (scale y) fillSize fillSize
                scale n     = (n - 1) * cellSize
                color On    = SDL.Pixel 0x00FFFFFF
                color Dying = SDL.Pixel 0x00888888
                color Off   = SDL.Pixel 0x00000000
