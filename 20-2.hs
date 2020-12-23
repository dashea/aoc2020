{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception (Exception)
import           Control.Monad (guard, void)
import           Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.Array as Array
import           Data.Array (Array, (!), (//), array)
import           Data.Bifunctor (second)
import           Data.Functor (($>))
import           Data.Ix (Ix, inRange)
import           Data.List (delete, nub, transpose)
import           Data.List.Safe ((!!), head, init, last, tail)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Text.Parsec
import           Text.Parsec.Text

import Prelude hiding ((!!), head, init, last, tail)

data Pixel = Set | Unset | Monster | MonsterSet | MonsterUnset
 deriving (Eq, Show)

data Border = TopBorder | BottomBorder | LeftBorder | RightBorder

type Tile a = (Int, [[a]])

type Grid a = [[Tile a]]

data AoCException = ArrayIndexOutOfBounds
                  | ParseError
 deriving (Eq, Show)

instance Exception AoCException

-- safe-ish array index, will still bomb out if accessing an unset index in a sparse array
(!?) :: (Ix i, MonadThrow m) => Array i e -> i -> m e
(!?) a i =
    if inRange (Array.bounds a) i then return (a ! i) else throwM ArrayIndexOutOfBounds

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

removeBorders :: Tile a -> Tile a
removeBorders (i, ps) =
    let newPixels = tail ps >>= init >>= mapM tail >>= mapM init
     in (i, fromMaybe [] newPixels)

combineGrid :: Grid a -> [[a]]
combineGrid g =
    let pixelsOnly = map (map snd) g
     in foldl addRows [] pixelsOnly
 where
    addRows :: [[a]] -> [[[a]]] -> [[a]]
    addRows acc ts =
        let pixelRow = concat $ fromMaybe [] $ mapM head ts
            remaining = fromMaybe [] $ mapM tail ts
         in if null pixelRow then acc else addRows (acc ++ [pixelRow]) remaining

toArray :: [[a]] -> Array (Int, Int) a
toArray input =
    let points = concat $ zipWith (\row y -> zipWith (\col x -> ((x, y), col)) row [0..]) input [0..]
        yMax = length input - 1
        xMax = length (fromMaybe [] (head input)) - 1
     in array ((0, 0), (xMax, yMax)) points

seaMonster :: ([Bool], [Bool], [Bool])
seaMonster =
    let (m1, m2, m3) = ("                  # "
                       ,"#    ##    ##    ###"
                       ," #  #  #  #  #  #   "
                       )
     in (map monsterPixel m1, map monsterPixel m2, map monsterPixel m3)
 where
    monsterPixel '#' = True
    monsterPixel _   = False

markMonsters :: Array (Int, Int) Pixel -> Array (Int, Int) Pixel
markMonsters input = foldl markMonster input (Array.range (Array.bounds input))
 where
    markMonster :: Array (Int, Int) Pixel -> (Int, Int) -> Array (Int, Int) Pixel
    markMonster input' coord =
        if hasMonster input' coord then setMonster input' coord else input'

    hasMonster :: Array (Int, Int) Pixel -> (Int, Int) -> Bool
    hasMonster input' (x, y) =
        let (m1, m2, m3) = seaMonster
         in matchRow input' (x, y) m1
         && matchRow input' (x, y+1) m2
         && matchRow input' (x, y+2) m3

    setMonster :: Array (Int, Int) Pixel -> (Int, Int) -> Array (Int, Int) Pixel
    setMonster input' (x, y) =
        let (m1, m2, m3) = seaMonster
            monsterPixels = setRow input' (x, y) m1 ++ setRow input' (x, y+1) m2 ++ setRow input' (x, y+2) m3
         in input' // monsterPixels

    setRow :: Array (Int, Int) Pixel -> (Int, Int) -> [Bool] -> [((Int, Int), Pixel)]
    setRow _ _ [] = []
    setRow input' p@(x, y) (m:ms) =
        let newPixel = case (input' !? p, m) of
                         (Just _, True) -> Monster
                         (Just Set, False) -> MonsterSet
                         (Just Unset, False) -> MonsterUnset
                         (_, _) -> MonsterUnset -- shouldn't happen
         in (p, newPixel) : setRow input' (x+1, y) ms

    matchRow :: Array (Int, Int) Pixel -> (Int, Int) -> [Bool] -> Bool
    matchRow _ _ [] = True
    matchRow input' p@(x, y) (m:ms) =
        case (input' !? p, m) of
          -- If the monster is "#" at this location, the image must be #.
          -- If the monster is " ", the image can be Set or Unset
          -- "Nothing" for the pixel indicates we ran out of image.
          (Just Set, True) -> matchRow input' (x+1, y) ms
          (Just Set, False) -> matchRow input' (x+1, y) ms
          (Just Unset, False) -> matchRow input' (x+1, y) ms
          _ -> False

-- getCorners :: Grid a -> Int
-- getCorners grid =
--     let topLeft = maybe 0 fst (head grid >>= head)
--         topRight = maybe 0 fst (head grid >>= last)
--         bottomLeft = maybe 0 fst (last grid >>= head)
--         bottomRight = maybe 0 fst (last grid >>= last)
--     in topLeft * topRight * bottomLeft * bottomRight

main :: IO ()
main = do
    tileData <- T.splitOn "\n\n" <$> TIO.getContents
    parsedTiles <- either (fail . show) return $ mapM (parse parseTile "input") tileData

    -- require a square grid
    let rowLen = floor (sqrt $ fromIntegral $ length parsedTiles :: Double)
    guard $ length parsedTiles == rowLen * rowLen

    grid <- maybe (fail "no image possible") return $ buildGrid rowLen parsedTiles

    let borderless = map (map removeBorders) grid
        seaArray = toArray (combineGrid borderless)
        withMonsters = markMonsters seaArray
    print $ length $ filter (\x -> x == Set || x == MonsterSet) $ Array.elems withMonsters
