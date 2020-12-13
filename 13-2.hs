{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Read (readMaybe)

readOrFail :: (Read a, MonadFail m) => String -> m a
readOrFail input = maybe (fail $ "Invalid input: " ++ input) return $ readMaybe input

parseInput :: MonadFail m => Text -> m [(Int, Int)]
parseInput input =
    case T.lines input of
      [_, l2] -> do
          buses <- mapM readBus (T.splitOn "," l2)
          return $ mapMaybe unpack $ zip buses [0..]

      _ -> fail "Invalid input"

 where
    readBus :: MonadFail m => Text -> m (Maybe Int)
    readBus "x" = return Nothing
    readBus bus = Just <$> readOrFail (T.unpack bus)

    unpack :: (Maybe a, b) -> Maybe (a, b)
    unpack (Just x, y) = Just (x, y)
    unpack (Nothing, _) = Nothing

-- earliestBus :: Int -> [Int] -> (Int, Int)
-- earliestBus time buses =
--     let departures = map (\b -> ((time + b) `div` b) * b) buses
--         busesWithDepartures = zip buses departures
--      in minimumBy (comparing snd) busesWithDepartures

earliestBus :: (Int, Int) -> (Int, Int) -> (Int, Int)
earliestBus (prevEarliest, prevLcm) (busId, offset) =
    let nextLcm = lcm prevLcm busId
        prevBuses = [prevEarliest + (x * prevLcm) | x <- [0..]]
        nextPossibles = filter (\x -> ((x + offset) `mod` busId) == 0) prevBuses
     in (head nextPossibles, nextLcm)

main :: IO ()
main = do
    buses <- TIO.getContents >>= parseInput
    print $ fst $ foldl earliestBus (0, 1) buses
