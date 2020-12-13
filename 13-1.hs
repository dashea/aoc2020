{-# LANGUAGE OverloadedStrings #-}

import           Data.List (minimumBy)
import           Data.Maybe (catMaybes)
import           Data.Ord (comparing)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Read (readMaybe)

readOrFail :: (Read a, MonadFail m) => String -> m a
readOrFail input = maybe (fail $ "Invalid input: " ++ input) return $ readMaybe input

parseInput :: MonadFail m => Text -> m (Int, [Int])
parseInput input =
    case T.lines input of
      [l1, l2] -> do
          earliestTime <- readOrFail (T.unpack l1)
          buses <- catMaybes <$> mapM readBus (T.splitOn "," l2)
          return (earliestTime, buses)

      _ -> fail "Invalid input"

 where
    readBus :: MonadFail m => Text -> m (Maybe Int)
    readBus "x" = return Nothing
    readBus bus = Just <$> readOrFail (T.unpack bus)

earliestBus :: Int -> [Int] -> (Int, Int)
earliestBus time buses =
    let departures = map (\b -> ((time + b) `div` b) * b) buses
        busesWithDepartures = zip buses departures
     in minimumBy (comparing snd) busesWithDepartures

main :: IO ()
main = do
    (time, buses) <- TIO.getContents >>= parseInput
    let (busId, departureTime) = earliestBus time buses
    print $ busId * (departureTime - time)
