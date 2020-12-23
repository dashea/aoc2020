{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad (guard, void)
import           Data.Bifunctor (second)
import           Data.Functor (($>))
import           Data.List (delete, nub, transpose)
import           Data.List.Safe ((!!), head, init, last)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Text.Parsec
import           Text.Parsec.Text

import Prelude hiding ((!!), head, init, last)

data Pixel = Set | Unset
 deriving (Eq, Show)

data Border = TopBorder | BottomBorder | LeftBorder | RightBorder

type Tile a = (Int, [[a]])

type Grid a = [[Tile a]]

flipX :: Tile a -> Tile a
flipX = second (map reverse)

flipY :: Tile a -> Tile a
flipY = second reverse

-- clockwise
rotate :: Tile a -> Tile a
rotate = second (map reverse . transpose)

allOrientations :: Eq a => Tile a -> [Tile a]
allOrientations t =
    let rotations = take 4 (iterate rotate t)
     in nub $ rotations ++ map flipX rotations ++ map flipY rotations

buildGrid :: forall a . Eq a => Int -> [Tile a] -> Maybe (Grid a)
buildGrid rowLen tiles = tryTile tiles tiles []
 where
    tryTile :: [Tile a] -> [Tile a] -> Grid a -> Maybe (Grid a)
    tryTile _ [] grid = Just grid
    tryTile [] _ _ = Nothing
    tryTile (t:ts) pool grid =
        let remaining = delete t pool
         in case tryOrientations (allOrientations t) remaining grid of
              Nothing -> tryTile ts pool grid
              Just g  -> Just g

    tryOrientations :: [Tile a] -> [Tile a] -> Grid a -> Maybe (Grid a)
    tryOrientations [] _ _ = Nothing
    tryOrientations (t:ts) pool grid =
        case tryOrientation pool grid t of
          Nothing -> tryOrientations ts pool grid
          Just g  -> Just g

    tryOrientation :: [Tile a] -> Grid a -> Tile a -> Maybe (Grid a)
    -- first tile, always fits
    tryOrientation pool [] t = tryTile pool pool [[t]]

    -- only one row so far
    tryOrientation pool grid@[row] t =
        if length row == rowLen
           then nextRow pool grid t
           else do
               prevTile <- last row
               guard $ matchBorder RightBorder prevTile t
               tryTile pool pool [row ++ [t]]

    tryOrientation pool grid t = do
        lastRow <- last grid
        prevRow <- init grid >>= last
        if length lastRow == rowLen
           then nextRow pool grid t
           else do
               prevRows <- init grid
               prevTile <- last lastRow
               prevRowTile <- prevRow !! length lastRow
               guard $ matchBorder RightBorder prevTile t
               guard $ matchBorder BottomBorder prevRowTile t
               tryTile pool pool $ prevRows ++ [lastRow ++ [t]]

    nextRow :: [Tile a] -> Grid a -> Tile a -> Maybe (Grid a)
    nextRow pool grid t = do
        lastRow <- last grid
        prevRowTile <- head lastRow
        guard $ matchBorder BottomBorder prevRowTile t
        tryTile pool pool $ grid ++ [[t]]

oppositeBorder :: Border -> Border
oppositeBorder TopBorder = BottomBorder
oppositeBorder BottomBorder = TopBorder
oppositeBorder LeftBorder = RightBorder
oppositeBorder RightBorder = LeftBorder

getBorder :: Border -> Tile a -> [a]
getBorder TopBorder (_, ps) = fromMaybe [] $ head ps
getBorder BottomBorder (_, ps) = fromMaybe [] $ last ps
getBorder LeftBorder (_, ps) = fromMaybe [] $ mapM head ps
getBorder RightBorder (_, ps) = fromMaybe [] $ mapM last ps

matchBorder :: Eq a => Border -> Tile a -> Tile a -> Bool
matchBorder border a b = getBorder border a == getBorder (oppositeBorder border) b

parseTile :: Parser (Tile Pixel)
parseTile = do
    header <- (string "Tile" >> spaces) *> (read <$> many1 digit)
    void $ char ':'
    void newline

    pixels <- parsePixels
    return (header, pixels)
 where
    parsePixels :: Parser [[Pixel]]
    parsePixels = many1 parseRow

    parseRow :: Parser [Pixel]
    parseRow = many1 parsePixel <* optional newline

    parsePixel :: Parser Pixel
    parsePixel = (char '#' $> Set) <|> (char '.' $> Unset)

getCorners :: Grid a -> Int
getCorners grid =
    let topLeft = maybe 0 fst (head grid >>= head)
        topRight = maybe 0 fst (head grid >>= last)
        bottomLeft = maybe 0 fst (last grid >>= head)
        bottomRight = maybe 0 fst (last grid >>= last)
    in topLeft * topRight * bottomLeft * bottomRight

main :: IO ()
main = do
    tileData <- T.splitOn "\n\n" <$> TIO.getContents
    parsedTiles <- either (fail . show) return $ mapM (parse parseTile "input") tileData

    -- require a square grid
    let rowLen = floor (sqrt $ fromIntegral $ length parsedTiles :: Double)
    guard $ length parsedTiles == rowLen * rowLen

    grid <- maybe (fail "no image possible") return $ buildGrid rowLen parsedTiles
    print $ getCorners grid
