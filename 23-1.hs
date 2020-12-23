doTurn :: Int -> Int -> [Int] -> [Int]
doTurn lowest highest input =
    let currentCup = head input
        cupsToTake = take 3 $ tail input
        newInput = currentCup : drop 3 (tail input)
        targetCup = destination (currentCup - 1) cupsToTake
        newCircle = placeCups [] targetCup cupsToTake newInput
     in take (length input) $ nextHead currentCup $ cycle newCircle
 where
    destination :: Int -> [Int] -> Int
    destination currentCup taken
      | currentCup < lowest = destination highest taken
      | currentCup `elem` taken = destination (currentCup - 1) taken
      | otherwise = currentCup

    placeCups :: [Int] -> Int -> [Int] -> [Int] -> [Int]
    placeCups acc target cupsToPlace input' =
        let current = head input'
            acc' = acc ++ [current]
         in if current == target
               then acc' ++ cupsToPlace ++ tail input'
               else placeCups acc' target cupsToPlace (tail input')

    nextHead :: Int -> [Int] -> [Int]
    nextHead currentCup input' =
        if head input' == currentCup then tail input' else nextHead currentCup (tail input')

finishCups :: [Int] -> [Int]
finishCups input = take (length input - 1) $ tail $ findOne $ cycle input
 where
    findOne input' = if head input' == 1 then input' else findOne (tail input')

main :: IO ()
main = do
    let startingCups = map (\c -> read [c]) "952438716"
        lowest = minimum startingCups
        highest = maximum startingCups
        endingCups = iterate (doTurn lowest highest) startingCups !! 100
    print $ concatMap show $ finishCups endingCups
