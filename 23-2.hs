import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap, (!))

doTurn :: Int -> Int -> Int -> IntMap Int -> (Int, IntMap Int)
doTurn lowest highest current input =
    let take1 = input ! current
        take2 = input ! take1
        take3 = input ! take2
        takeNext = input ! take3
        targetCup = destination (current - 1) [take1, take2, take3]

        cupsRemoved = IntMap.insert current takeNext input
        cupsAdded1 = IntMap.insert targetCup take1 cupsRemoved
        cupsAdded2 = IntMap.insert take3 (input ! targetCup) cupsAdded1
     in (cupsAdded2 ! current, cupsAdded2)
 where
    destination :: Int -> [Int] -> Int
    destination currentCup taken
      | currentCup < lowest = destination highest taken
      | currentCup `elem` taken = destination (currentCup - 1) taken
      | otherwise = currentCup

makeCups :: [Int] -> IntMap Int
makeCups [] = error "Empty list"
makeCups (x:xs) = makeCups' IntMap.empty x xs
 where
    makeCups' acc cur (x':xs') =
        let acc' = IntMap.insert cur x' acc
         in makeCups' acc' x' xs'
    makeCups' acc cur [] = IntMap.insert cur x acc

main :: IO ()
main = do
    let startingCups = map (\c -> read [c]) "952438716"
        extendedCups = startingCups ++ [(maximum startingCups + 1) .. 1000000]
        lowest = minimum extendedCups
        highest = maximum extendedCups
        circle = makeCups extendedCups

    let (_, endingCups) = iterate (uncurry (doTurn lowest highest)) (head startingCups, circle) !! 10000000
        a = endingCups ! 1
        b = endingCups ! a
    print $ a * b
