import           Data.List (inits)
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty(..))

sums :: NonEmpty Int -> [Int]
sums input = concat $ NE.zipWith (\i -> map (+i)) input (NE.tails input)

firstNotSum :: Int -> [Int] -> Maybe Int
firstNotSum preambleLength input = do
    (test:|preamble) <- NE.nonEmpty $ reverse $ take (preambleLength + 1) input
    preamble' <- NE.nonEmpty preamble
    if test `elem` sums preamble'
      then firstNotSum preambleLength (drop 1 input)
      else return test

findRange :: Int -> [Int] -> Maybe [Int]
findRange _ [] = Nothing
findRange target input =
    case filter (\l -> sum l == target) (inits input) of
      [] -> findRange target (drop 1 input)
      (x:_) -> Just x

main :: IO ()
main = do
    input <- map read . lines <$> getContents
    range <- maybe (fail "no answer found") return $ firstNotSum 25 input >>= \t -> findRange t input
    let smallest = minimum range
        largest  = maximum range
    print $ smallest + largest
