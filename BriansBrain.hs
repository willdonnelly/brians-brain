import Data.List

data CellState = CellOff | CellDying | CellOn deriving (Eq, Ord, Enum, Show)
data Neighbors = Neighbors CellState [CellState] deriving (Eq, Show)
data World     = World Int Int [[CellState]]

wrapCoord :: World -> (Int, Int) -> (Int, Int)
wrapCoord (World mx my cells) (x, y) = (wrapNum mx x, wrapNum my y)
  where wrapNum max val
            | val > max  = val - max
            | val < 0    = val + max
            | otherwise  = val

getNeighbors :: World -> (Int, Int) -> Neighbors
getNeighbors world (x, y) = Neighbors center [nw,n,ne,w,e,sw,s,se]
  where [nw,n,ne,w,center,e,sw,s,se] = map cellVal offsets
        offsets = [ (x + a, y + b) | a <- [-1..1], b <- [-1..1] ]
        cellVal = getCell world . wrapCoord world
        getCell (World mx my cells) (x, y) = cells !! x !! y

stepWorld :: World -> World
stepWorld world@(World mx my _) = World mx my $ split (mx + 1) cells
  where cells   = [ newCell (x,y) | x <- [0..mx], y <- [0..my] ]
        newCell = stepCell . countNeighbors . getNeighbors world
        split n = unfoldr splitNext
          where splitNext [] = Nothing
                splitNext xs = Just $ splitAt n xs

countNeighbors :: Neighbors -> (CellState, Int)
countNeighbors (Neighbors c cs) = (c, numLiving cs)
  where numLiving = length . filter (== CellOn)

stepCell :: (CellState, Int) -> CellState
stepCell (CellOff,   2) = CellOn
stepCell (CellOff,   _) = CellOff
stepCell (CellDying, _) = CellOff
stepCell (CellOn,    _) = CellDying

printWorld (World mx my cells) = unlines . map showRow $ cells
  where showRow = concatMap showCell
        showCell CellOn    = "#"
        showCell CellDying = "+"
        showCell CellOff   = "."

testWorld :: World
testWorld = World 2 2 cells
  where cells = [ [CellOff, CellOn , CellOff]
                , [CellOff, CellOff, CellOff]
                , [CellOff, CellOn , CellOff]
                ]
