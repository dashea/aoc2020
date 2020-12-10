import qualified Data.IntSet as IS
import           Data.IntSet (IntSet)
import           Data.Maybe (mapMaybe)

tryAdapter :: Int -> IntSet -> Maybe [Int]
tryAdapter currentJoltage adapters =
    let possibleJoltages = map ($ currentJoltage) [(+1), (+2), (+3)]
        possibleAdapters = filter (`IS.member` adapters) possibleJoltages
        possibleResults = mapMaybe tryJolt possibleAdapters
     in if IS.null adapters
          then Just [currentJoltage]
          else case possibleResults of
                 (x:_) -> Just x
                 []    -> Nothing
 where
    tryJolt :: Int -> Maybe [Int]
    tryJolt jolt = (currentJoltage:) <$> tryAdapter jolt (IS.delete jolt adapters)

countOnesAndThrees :: (Int, Int) -> [Int] -> (Int, Int)
countOnesAndThrees (ones, threes) (x1:x2:xs) =
    case x2 - x1 of
      1 -> countOnesAndThrees (ones+1, threes) (x2:xs)
      3 -> countOnesAndThrees (ones, threes+1) (x2:xs)
      _ -> countOnesAndThrees (ones, threes) (x2:xs)
countOnesAndThrees acc _ = acc

main :: IO ()
main = do
    input <- map read . lines <$> getContents
    let adapters = IS.fromList input

    adapterChain <- maybe (fail "no answer found") return $ tryAdapter 0 adapters
    let (ones, threes) = countOnesAndThrees (0, 0) adapterChain

    -- add one to threes for the difference between the last adapter and the device
    print $ ones * (threes + 1)
