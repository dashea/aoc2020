import           Data.List (sort)

countOnesAndThrees :: MonadFail m => (Int, Int) -> [Int] -> m (Int, Int)
countOnesAndThrees (ones, threes) (x1:x2:xs) =
    case x2 - x1 of
      1 -> countOnesAndThrees (ones+1, threes) (x2:xs)
      2 -> countOnesAndThrees (ones, threes) (x2:xs)
      3 -> countOnesAndThrees (ones, threes+1) (x2:xs)
      x -> fail $ "Invalid transition: " ++ show x
countOnesAndThrees acc _ = return acc

main :: IO ()
main = do
    input <- map read . lines <$> getContents

    let adapterChain = sort (0:input)
    (ones, threes) <- countOnesAndThrees (0, 0) adapterChain

    -- add one to threes for the difference between the last adapter and the device
    print $ ones * (threes + 1)
