import           Control.Exception (Exception)
import           Control.Monad (unless)
import           Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.Array as Array
import           Data.Array (Array, (!), (//), array)
import           Data.Ix (Ix, index, inRange, range, rangeSize)
import           Data.Maybe (fromMaybe)

data AoCException = ArrayIndexOutOfBounds
                  | ParseError
 deriving (Eq, Show)

instance Exception AoCException

data Cube = Active | Inactive
 deriving (Eq, Show)

data Point = Point
    { _xCoord :: Int
    , _yCoord :: Int
    , _zCoord :: Int
    }
 deriving (Eq, Show)

instance Ord Point where
    compare (Point x1 y1 z1) (Point x2 y2 z2)
      | z1 /= z2 = z1 `compare` z2
      | y1 /= y2 = y1 `compare` y2
      | otherwise = x1 `compare` x2

instance Ix Point where
    range (Point x1 y1 z1, Point x2 y2 z2) = [Point x y z | z <- [z1..z2], y <- [y1..y2], x <- [x1..x2]]

    index (Point xmin ymin zmin, Point xmax ymax zmax) (Point x y z) =
        let xidx = index (xmin, xmax) x
            yidx = index (ymin, ymax) y
            zidx = index (zmin, zmax) z
            xSize = rangeSize (xmin, xmax)
            ySize = rangeSize (ymin, ymax)
         in xidx + (yidx * xSize) + (zidx * xSize * ySize)

    inRange (Point xmin ymin zmin, Point xmax ymax zmax) (Point x y z) =
        (x >= xmin) && (x <= xmax) && (y >= ymin) && (y <= ymax) && (z >= zmin) && (z <= zmax)

type Space = Array Point Cube

-- safe-ish array index, will still bomb out if accessing an unset index in a sparse array
(!?) :: (Ix i, MonadThrow m) => Array i e -> i -> m e
(!?) a i =
    -- liftIO $ traceIO $ "Bounds: " ++ show (Array.bounds a)
    -- liftIO $ traceIO $ "In range: " ++ show i ++ ", " ++ show (inRange (Array.bounds a) i)
    if inRange (Array.bounds a) i then return (a ! i) else throwM ArrayIndexOutOfBounds

getCube :: Space -> Point -> Cube
getCube s p = fromMaybe Inactive $ s !? p

getNeighbors :: Space -> Point -> [Cube]
getNeighbors s p =
    let deltas = [0, 1, -1]
        directions = [((+x), (+y), (+z)) | x <- deltas,
                                           y <- deltas,
                                           z <- deltas,
                                           not ((x == 0) && (y == 0) && (z == 0))]
        neighboringPoints = map (applyDirection p) directions
     in map (getCube s) neighboringPoints
 where
    applyDirection (Point x y z) (dx, dy, dz) = Point (dx x) (dy y) (dz z)

addBorders :: Space -> Space
addBorders s =
    let (Point xmin ymin zmin, Point xmax ymax zmax) = Array.bounds s
        newRange = (Point (xmin - 1) (ymin - 1) (zmin - 1), Point (xmax + 1) (ymax + 1) (zmax + 1))
        emptySpace = Array.listArray newRange $ repeat Inactive
     in emptySpace // Array.assocs s

doConway :: Space -> Space
doConway s =
    let withBorders = addBorders s
     in array (Array.bounds withBorders) $ map doCube $ Array.assocs withBorders
 where
    doCube :: (Point, Cube) -> (Point, Cube)
    doCube (p, c) =
        let activeNeighbors = length $ filter (== Active) $ getNeighbors s p
            nextState = case c of
                          Active ->   if (activeNeighbors == 2) || (activeNeighbors == 3) then Active else Inactive
                          Inactive -> if activeNeighbors == 3 then Active else Inactive
         in (p, nextState)

parsePlane :: (MonadThrow m, MonadFail m) => [String] -> m Space
parsePlane input = do
    plane@(row:rows) <- mapM parseRow input
    let rowLen = length row
    unless (all ((== rowLen) . length) rows) $ throwM ParseError

    return $ Array.listArray (Point 0 0 0, Point (rowLen - 1) (length plane - 1) 0) $ concat plane
 where
    parseRow :: MonadThrow m => String -> m [Cube]
    parseRow = mapM parseCube

    parseCube :: MonadThrow m => Char -> m Cube
    parseCube '.' = return Inactive
    parseCube '#' = return Active
    parseCube _   = throwM ParseError

main :: IO ()
main = do
    initialSpace <- (lines <$> getContents) >>= parsePlane
    let conways = iterate doConway initialSpace
        sixth = conways !! 6
        numActive = foldl (\acc x -> if x == Active then acc + 1 else acc) (0::Int) sixth

    print numActive
