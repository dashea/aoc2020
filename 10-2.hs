import           Data.List (sort)
import           Data.List.Split (dropFinalBlank, keepDelimsR, split, whenElt)

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
    let adapterChain = sort (0:input)

    let diffPairs = zip (diffs adapterChain ++ [3]) adapterChain
    let splitOnThrees = map (map snd) $ (split . dropFinalBlank . keepDelimsR . whenElt) ((== 3) . fst) diffPairs
    print $ product $ map (length . allPossibilities) splitOnThrees
