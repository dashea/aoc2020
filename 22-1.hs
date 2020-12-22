{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

playGame :: MonadFail m => [Int] -> [Int] -> m [Int]
playGame playerOne [] = return playerOne
playGame [] playerTwo = return playerTwo
playGame (p1:p1s) (p2:p2s)
  | p1 > p2 = playGame (p1s ++ [p1, p2]) p2s
  | p2 > p1 = playGame p1s (p2s ++ [p2, p1])
  | otherwise = fail "no ties allowed"

finalScore :: [Int] -> Int
finalScore deck = sum $ zipWith (*) [1..] (reverse deck)

main :: IO ()
main = do
    [_p1Header : p1Cards, _p2Header : p2Cards] <- map T.lines . T.splitOn "\n\n" <$> TIO.getContents
    let p1Deck = map (read . T.unpack) p1Cards
        p2Deck = map (read . T.unpack) p2Cards
    winningDeck <- playGame p1Deck p2Deck
    print $ finalScore winningDeck
