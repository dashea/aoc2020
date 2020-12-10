import qualified Data.IntSet as IS
import           Data.IntSet (IntSet)
import           Data.List.Split (dropFinalBlank, keepDelimsR, split, whenElt)
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

allPossibilities :: [Int] -> [[Int]]
allPossibilities (x1:x2:x3:xs) =
    let noSkip = map (x1:) $ allPossibilities (x2:x3:xs)
        skip   = allPossibilities (x1:x3:xs)
     in if (x3 - x1) <= 3
          then skip ++ noSkip
          else noSkip
allPossibilities input = [input]

diffs :: [Int] -> [Int]
diffs (x1:x2:xs) = (x2 - x1):diffs (x2:xs)
diffs _ = []

main :: IO ()
main = do
    input <- map read . lines <$> getContents
    let adapters = IS.fromList input

    adapterChain <- maybe (fail "no answer found") return $ tryAdapter 0 adapters

    print adapterChain
    print $ diffs adapterChain
    let diffPairs = zip (diffs adapterChain ++ [3]) adapterChain
    let splitOnThrees = map (map snd) $ (split . dropFinalBlank . keepDelimsR . whenElt) ((== 3) . fst) diffPairs
    print $ product $ map (length . allPossibilities) splitOnThrees
