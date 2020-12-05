import           Control.Monad (guard)
import qualified Data.IntSet as IS
import           Data.Ix (range, rangeSize)
import           Data.List.Safe (head)

import Prelude hiding (head)

data Partition = Lower | Upper
type Seat = ([Partition], [Partition])

rowRange :: (Int, Int)
rowRange = (0, 127)

colRange :: (Int, Int)
colRange = (0, 7)

binaryPartition :: (Int, Int) -> Partition -> (Int, Int)
binaryPartition r@(l, u) Lower =
    let u' = u - (rangeSize r `div` 2)
     in (l, u')
binaryPartition r@(l, u) Upper =
    let l' = l + (rangeSize r `div` 2)
     in (l', u)

parseSeat :: MonadFail m => String -> m Seat
parseSeat [x1, x2, x3, x4, x5, x6, x7, y1, y2, y3] = do
    fb <- mapM parseFB [x1, x2, x3, x4, x5, x6, x7]
    lr <- mapM parseLR [y1, y2, y3]
    return (fb, lr)
 where
    parseFB 'F' = return Lower
    parseFB 'B' = return Upper
    parseFB x = fail $ "Invalid front/back specification: " ++ show x

    parseLR 'L' = return Lower
    parseLR 'R' = return Upper
    parseLR x = fail $ "Invalid left/right specification: " ++ show x

parseSeat s = fail $ "Invalid seat: " ++ s

seatToRowCol :: MonadFail m => Seat -> m (Int, Int)
seatToRowCol (fb, lr) = do
    let (rowL, rowU) = foldl binaryPartition rowRange fb
        (colL, colU) = foldl binaryPartition colRange lr
    row <- if rowL == rowU then return rowL else fail "Binary partition failed"
    col <- if colL == colU then return colL else fail "Binary partition failed"
    return (row, col)

rowColToSeatId :: (Int, Int) -> Int
rowColToSeatId (row, col) = (row * 8) + col

main :: IO ()
main = do
    seats <- (lines <$> getContents) >>= mapM parseSeat >>= mapM seatToRowCol
    -- print $ maximum $ map rowColToSeatId seats

    -- there's only 1024 possible seats, so just use a intset to quickly find the holes in the range
    let allSeats = IS.fromList $ [rowColToSeatId (row, col) | row <- range rowRange, col <- range colRange]
        takenSeats = IS.fromList $ map rowColToSeatId seats
        availableSeats = IS.filter (adjacentsExist takenSeats) $ IS.difference allSeats takenSeats

    guard $ IS.size availableSeats == 1

    head (IS.toList availableSeats) >>= print
 where
    adjacentsExist taken seatId =
        IS.member (seatId - 1) taken && IS.member (seatId + 1) taken
