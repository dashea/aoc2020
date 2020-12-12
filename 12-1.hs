import Control.Monad.State (State, evalState, get, modify)
import Text.Read (readMaybe)

data Movement = North Int
              | South Int
              | East Int
              | West Int
              | TurnLeft Int
              | TurnRight Int
              | Forward Int

data Vec = Vec Int Int

type Point = (Int, Int)

turn :: Int -> Int -> Int
turn current change = normalize (current + change)
 where
    normalize :: Int -> Int
    normalize input =
        if input < 0
           then normalize (input + 360)
           else input `mod` 360

toCartesian :: Vec -> Point
toCartesian (Vec deg r) =
    let theta = (realToFrac deg :: Double) * (pi / 180.0)
        x     = round $ realToFrac r * cos theta
        y     = round $ realToFrac r * sin theta
     in (x, y)

moveIt :: Int -> Point -> [Movement] -> Point
moveIt startingAngle startingPoint moves =
    evalState (moveIt' startingPoint moves) startingAngle
 where
    moveIt' :: Point -> [Movement] -> State Int Point
    moveIt' (x, y) ((North r):ms) = moveIt' (x, y + r) ms
    moveIt' (x, y) ((South r):ms) = moveIt' (x, y - r) ms
    moveIt' (x, y) ((East r):ms)  = moveIt' (x + r, y) ms
    moveIt' (x, y) ((West r):ms)  = moveIt' (x - r, y) ms
    moveIt' p ((TurnLeft d):ms) = modify (`turn` d) >> moveIt' p ms
    moveIt' p ((TurnRight d):ms) = modify (`turn` (-d)) >> moveIt' p ms
    moveIt' (x, y) ((Forward r):ms) = do
        theta <- get
        let (x', y') = toCartesian $ Vec theta r
        moveIt' (x + x', y + y') ms
    moveIt' p [] = return p

manhattanDistance :: Point -> Int
manhattanDistance (x, y) = abs x + abs y

readOrFail :: (Read a, MonadFail m) => String -> m a
readOrFail input = maybe (fail $ "Invalid input: " ++ input) return $ readMaybe input

parseInput :: MonadFail m => String -> m Movement
parseInput ('N':xs) = North <$> readOrFail xs
parseInput ('S':xs) = South <$> readOrFail xs
parseInput ('E':xs) = East <$> readOrFail xs
parseInput ('W':xs) = West <$> readOrFail xs
parseInput ('L':xs) = TurnLeft <$> readOrFail xs
parseInput ('R':xs) = TurnRight <$> readOrFail xs
parseInput ('F':xs) = Forward <$> readOrFail xs
parseInput x = fail $ "Invalid input: " ++ x

main :: IO ()
main = do
    movements <- (lines <$> getContents) >>= mapM parseInput
    let startingAngle = 0
        startingPoint = (0, 0)
    print $ manhattanDistance $ moveIt startingAngle startingPoint movements
