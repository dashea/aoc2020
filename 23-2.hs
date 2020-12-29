{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array (Array, array)
import Data.Array.IO (IOArray)
import Data.Array.MArray (MArray, getBounds, readArray, thaw, writeArray)
import Data.Ix (Ix)

doTurn :: forall a m i . (MArray a i m, Ix i, Num i) => i -> a i i -> m i
doTurn current input = do
    (lowest, highest) <- getBounds input
    take1 <- readArray input current
    take2 <- readArray input take1
    take3 <- readArray input take2
    takeNext <- readArray input take3
    let targetCup = destination lowest highest (current - 1) [take1, take2, take3]

    targetNext <- readArray input targetCup

    -- link "current" to the cup after the 3 taken
    writeArray input current takeNext
    -- link the destination to the first of the 3 cups taken
    writeArray input targetCup take1
    -- link the last of the 3 cups taken to the cup that was after the destination
    writeArray input take3 targetNext

    readArray input current
 where
    destination :: i -> i -> i -> [i] -> i
    destination lowest highest currentCup taken
      | currentCup < lowest = destination lowest highest highest taken
      | currentCup `elem` taken = destination lowest highest (currentCup - 1) taken
      | otherwise = currentCup

doTurns :: (MArray a i m, Ix i, Num i, Num j, Eq j) => j -> i -> a i i -> m ()
doTurns 0 _ _ = return ()
doTurns x current input = do
    next <- doTurn current input
    doTurns (x - 1) next input

makeCups :: [Int] -> Array Int Int
makeCups [] = error "Empty list"
makeCups input@(x:xs) = array (minimum input, maximum input) $ makeCups' [] x xs
 where
    makeCups' acc cur (x':xs') =
        let acc' = (cur, x') : acc
         in makeCups' acc' x' xs'
    makeCups' acc cur [] = (cur, x) : acc

main :: IO ()
main = do
    let startingCups = map (\c -> read [c]) "952438716"
        extendedCups = startingCups ++ [(maximum startingCups + 1) .. 1000000]
        circle = makeCups extendedCups

    mCircle <- thaw circle :: IO (IOArray Int Int)
    doTurns (10000000 :: Int) (head startingCups) mCircle

    a <- readArray mCircle 1
    b <- readArray mCircle a

    print $ a * b
