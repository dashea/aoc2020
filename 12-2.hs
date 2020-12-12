import Control.Monad.State (State, evalState, get, modify)
import Data.Bifunctor (first, second)
import Text.Read (readMaybe)

data Movement = North Int
              | South Int
              | East Int
              | West Int
              | TurnLeft Int
              | TurnRight Int
              | Forward Int

data Vec = Vec Double Double

type Point = (Int, Int)

turn :: (Ord a, Num a) => a -> a -> a
turn current change = normalize (current + change)
 where
    normalize input
      | input < 0 = normalize (input + 360)
      | input > 360 = normalize (input - 360)
      | otherwise = input

toCartesian :: Vec -> Point
toCartesian (Vec deg r) =
    let theta = deg * (pi / 180.0)
        x     = round $ r * cos theta
        y     = round $ r * sin theta
     in (x, y)

moveIt :: Point -> Point -> [Movement] -> Point
moveIt startingPoint startingWaypoint moves =
    evalState (moveIt' startingPoint moves) startingWaypoint
 where
    moveIt' :: Point -> [Movement] -> State Point Point
    moveIt' p ((North r):ms) = modify (second (+r)) >> moveIt' p ms
    moveIt' p ((South r):ms) = modify (second (subtract r)) >> moveIt' p ms
    moveIt' p ((East r):ms)  = modify (first (+r)) >> moveIt' p ms
    moveIt' p ((West r):ms)  = modify (first (subtract r)) >> moveIt' p ms
    moveIt' p ((TurnLeft d):ms) = modify (turnWaypoint d) >> moveIt' p ms
    moveIt' p ((TurnRight d):ms) = modify (turnWaypoint (-d)) >> moveIt' p ms

    moveIt' (x, y) ((Forward r):ms) = do
        (dx, dy) <- get
        let newPoint = (x + (dx * r), y + (dy * r))
        moveIt' newPoint ms

    moveIt' p [] = return p

    turnWaypoint :: Int -> Point -> Point
    turnWaypoint d (wx, wy) =
        let r = sqrt $ realToFrac ((wx * wx) + (wy * wy))
            theta = atan ((realToFrac wy :: Double) / (realToFrac wx :: Double))
            -- correct the atan value based on quadrant
            theta' = case (wx >= 0, wy >= 0) of
                       (True, True) -> theta
                       (False, True) -> theta + pi
                       (False, False) -> theta + pi
                       (True, False) -> theta + (2 * pi)
            angle = theta' * (180.0 / pi)
            newAngle = turn angle (realToFrac d)
         in toCartesian $ Vec newAngle r

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
    let startingPoint = (0, 0)
        startingWaypoint = (10, 1)
    print $ manhattanDistance $ moveIt startingPoint startingWaypoint movements
