import Control.Monad (guard)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

loop :: Integer -> Integer -> Integer
loop subject val = (val * subject) `mod` 20201227

crack :: Integer -> Integer -> Int
crack subject target =
    let starting = 1
     in fromJust $ elemIndex target $ iterate (loop subject) starting

main :: IO ()
main = do
    [cardKey, doorKey] <- map read . lines <$> getContents
    let cardLoopSize = crack 7 cardKey
        doorLoopSize = crack 7 doorKey
        cardEncKey = iterate (loop doorKey) 1 !! cardLoopSize
        doorEncKey = iterate (loop cardKey) 1 !! doorLoopSize

    guard $ cardEncKey == doorEncKey

    print doorEncKey
