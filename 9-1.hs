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

main :: IO ()
main = do
    input <- map read . lines <$> getContents
    maybe (fail "no answer found") return (firstNotSum 25 input) >>= print
