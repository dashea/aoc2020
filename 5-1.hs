import Data.Ix (rangeSize)

data Partition = Lower | Upper
type Seat = ([Partition], [Partition])

rowRange :: (Int, Int)
rowRange = (0, 127)

colRange :: (Int, Int)
colRange = (0, 7)

binaryPartition :: (Int, Int) -> Partition -> (Int, Int)
binaryPartition range@(l, u) Lower =
    let u' = u - (rangeSize range `div` 2)
     in (l, u')
binaryPartition range@(l, u) Upper =
    let l' = l + (rangeSize range `div` 2)
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
    print $ maximum $ map rowColToSeatId seats
