import Data.Vector (Vector, (!?), fromList)

data TreeSquare = Empty | Tree

type Forest = Vector (Vector TreeSquare)
type Point = (Int, Int)

parseTreeLine :: MonadFail m => String -> m (Vector TreeSquare)
parseTreeLine input = fromList <$> mapM parseChar input
 where
    parseChar '.' = return Empty
    parseChar '#' = return Tree
    parseChar x   = fail $ "Bad character: " ++ [x]

checkPoint :: Forest -> Point -> Maybe TreeSquare
-- treat each row as infinitely repeating
-- returns nothing when past the bottom of the forest
checkPoint forest (x, y) = forest !? y >>= (\r -> r !? (x `mod` length r))

-- returns the number of trees hit
travelSlope :: Forest -> (Point -> Point) -> Point -> Int
travelSlope forest slope currentPoint =
    let nextPoint = slope currentPoint
     in case checkPoint forest nextPoint of
          Nothing    -> 0
          Just Empty -> travelSlope forest slope nextPoint
          Just Tree  -> 1 + travelSlope forest slope nextPoint

main :: IO ()
main = do
    forest <- fromList <$> (getContents >>= mapM parseTreeLine . lines)
    -- part 1
    -- let startingPoint = (0, 0)
    --     slope (x, y) = (x + 3, y + 1)
    -- print $ travelSlope forest slope startingPoint

    let startingPoint = (0,0)
        pairToSlope (right, down) (x, y) = (x + right, y + down)
        slopes = map pairToSlope [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

    print $ product $ map (\s -> travelSlope forest s startingPoint) slopes
