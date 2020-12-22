{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HashSet as HashSet
import           Data.HashSet (HashSet)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Hand = [Int]
type Hands = (Hand, Hand)
data Player = Player1 | Player2
 deriving (Show)

playGame :: MonadFail m => Hands -> m (Player, Hands)
playGame = playRound HashSet.empty
 where
    playRound :: MonadFail m => HashSet Hands -> Hands -> m (Player, Hands)
    playRound history hands@(p1, p2) =
        if HashSet.member hands history
          then return (Player1, hands)
          else let history' = HashSet.insert hands history
                in case (p1, p2) of
                     ([], _) -> return (Player2, hands)
                     (_, []) -> return (Player1, hands)
                     (p1Card : p1s, p2Card : p2s)
                       | (p1Card <= length p1s) && (p2Card <= length p2s) -> do
                           (winningPlayer, _) <- playGame (take p1Card p1s, take p2Card p2s)
                           playRound history' (evalWinner p1Card p2Card p1s p2s winningPlayer)
                       | p1Card > p2Card -> playRound history' $ evalWinner p1Card p2Card p1s p2s Player1
                       | p2Card > p1Card -> playRound history' $ evalWinner p1Card p2Card p1s p2s Player2
                       | otherwise -> fail "no ties allowed"

    evalWinner :: Int -> Int -> [Int] -> [Int] -> Player -> Hands
    evalWinner p1Card p2Card p1s p2s Player1 = (p1s ++ [p1Card, p2Card], p2s)
    evalWinner p1Card p2Card p1s p2s Player2 = (p1s, p2s ++ [p2Card, p1Card])

finalScore :: [Int] -> Int
finalScore deck = sum $ zipWith (*) [1..] (reverse deck)

winningHand :: (Player, Hands) -> Hand
winningHand (Player1, (p1, _)) = p1
winningHand (Player2, (_, p2)) = p2

main :: IO ()
main = do
    [_p1Header : p1Cards, _p2Header : p2Cards] <- map T.lines . T.splitOn "\n\n" <$> TIO.getContents
    let p1Deck = map (read . T.unpack) p1Cards
        p2Deck = map (read . T.unpack) p2Cards
    winningDeck <- winningHand <$> playGame (p1Deck, p2Deck)
    print $ finalScore winningDeck
