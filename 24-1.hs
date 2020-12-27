import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)

-- point axes are on vertices, directions are on edges, hexagons are arranged with
-- vertices directly up and down, edges directly east and west
-- x-axis: WNW/ESE
-- y-axis: WSE/ENE
-- z-axis: N/S
type Point = (Int, Int, Int)

data Direction = East
               | SouthEast
               | SouthWest
               | West
               | NorthWest
               | NorthEast

data Color = Black | White
 deriving (Eq)

type Floor = HashMap Point Color

travel :: Point -> Direction -> Point
travel (x, y, z) East = (x+1, y+1, z)
travel (x, y, z) West = (x-1, y-1, z)
travel (x, y, z) SouthEast = (x+1, y, z-1)
travel (x, y, z) NorthWest = (x-1, y, z+1)
travel (x, y, z) SouthWest = (x, y-1, z-1)
travel (x, y, z) NorthEast = (x, y+1, z+1)

flipColor :: Color -> Color
flipColor Black = White
flipColor White = Black

-- if a tile is unknown, it's in its starting state (White) and flipping to Black
flipTile :: Floor -> Point -> Floor
flipTile fl point = HashMap.insertWith (const flipColor) point Black fl

parseInput :: MonadFail m => String -> m [Direction]
parseInput [] = return []
parseInput ('s':'e':rest) = (SouthEast:) <$> parseInput rest
parseInput ('s':'w':rest) = (SouthWest:) <$> parseInput rest
parseInput ('n':'e':rest) = (NorthEast:) <$> parseInput rest
parseInput ('n':'w':rest) = (NorthWest:) <$> parseInput rest
parseInput ('e':rest) = (East:) <$> parseInput rest
parseInput ('w':rest) = (West:) <$> parseInput rest
parseInput _ = fail "Bad input"

main :: IO ()
main = do
    input <- (lines <$> getContents) >>= mapM parseInput
    let refPoint = (0, 0, 0)
        inputPoints = map (foldl travel refPoint) input
        fl = foldl flipTile HashMap.empty inputPoints
        blackTiles = HashMap.filter (== Black) fl
    print $ HashMap.size blackTiles
