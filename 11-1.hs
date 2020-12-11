import           Data.Bifunctor (bimap)
import           Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import           Data.Vector (Vector, (!?))

data Square = Floor | Seat Bool
 deriving (Eq)

type Room = Vector (Vector Square)
type Point = (Int, Int)

getSquare :: Room -> Point -> Maybe Square
getSquare r (x, y) = r !? y >>= (!? x)

doConway :: Room -> Room
doConway r = V.imap doRow r
 where
    doRow :: Int -> Vector Square -> Vector Square
    doRow y = V.imap (doSquare y)

    doSquare :: Int -> Int -> Square -> Square
    doSquare _ _ Floor = Floor
    doSquare y x s@(Seat _) =
        let directions = [(subtract 1, id), (subtract 1, subtract 1), (id, subtract 1), ((+1), subtract 1),
                          ((+1), id), ((+1), (+1)), (id, (+1)), (subtract 1, (+1))]
            adjacents  = map (($ (x, y)) . uncurry bimap) directions
            occupied   = length $ filter (== Seat True) $ mapMaybe (getSquare r) adjacents
         in if occupied == 0
              then Seat True
              else if occupied >= 4
                then Seat False
                else s

runUntilStable :: Room -> Room
runUntilStable room =
    let room' = doConway room
     in if room == room'
          then room'
          else runUntilStable room'

parseRoom :: MonadFail m => [String] -> m Room
parseRoom input = V.fromList <$> mapM parseRow input
 where
    parseRow :: MonadFail m => String -> m (Vector Square)
    parseRow input' =  V.fromList <$> mapM parseSquare input'

    parseSquare :: MonadFail m => Char -> m Square
    parseSquare '.' = return Floor
    parseSquare 'L' = return (Seat False)
    parseSquare '#' = return (Seat True)
    parseSquare x   = fail $ "Invalid character: " ++ show x

main :: IO ()
main = do
    input <- (lines <$> getContents) >>= parseRoom
    print $ V.length $ V.filter (== Seat True) $ V.concat $ V.toList $ runUntilStable input
