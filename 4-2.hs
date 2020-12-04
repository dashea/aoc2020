{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
import           Control.Monad (guard, void)
import           Data.Attoparsec.Text.Lazy
import           Data.Char (isSpace)
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Numeric (readHex)
import           Text.Read (readMaybe)

import Prelude hiding (takeWhile)

data Height = HeightCm Int | HeightIn Int
 deriving (Show)

data RGB = RGB Int Int Int
 deriving (Show)

data EyeColor = Amber | Blue | Brown | Gray | Green | Hazel | Other
 deriving (Show)

data PassportField = Byr Int
                   | Iyr Int
                   | Eyr Int
                   | Hgt Height
                   | Hcl RGB
                   | Ecl EyeColor
                   | Pid Int
                   | Cid Text
 deriving (Show)

data Passport =
  Passport
    { _birthYear :: Int
    , _issueYear :: Int
    , _expirationYear :: Int
    , _height :: Height
    , _hairColor :: RGB
    , _eyeColor :: EyeColor
    , _passportId :: Int
    , _countryId :: Maybe Text
    } deriving (Show)

-- same as above but with everything optional, in order to build from a stream of pairs
data PassportBuilder =
  PassportBuilder
    { birthYear' :: Maybe Int
    , issueYear' :: Maybe Int
    , expirationYear' :: Maybe Int
    , height' :: Maybe Height
    , hairColor' :: Maybe RGB
    , eyeColor' :: Maybe EyeColor
    , passportId' :: Maybe Int
    , countryId' :: Maybe Text
    }

passportFieldParser :: Parser PassportField
passportFieldParser = do
    field <- takeWhile1 (/= ':')
    void $ char ':'
    fieldParser field
 where
    fieldParser :: Text -> Parser PassportField
    fieldParser "byr" = Byr <$> byrParser
    fieldParser "iyr" = Iyr <$> iyrParser
    fieldParser "eyr" = Eyr <$> eyrParser
    fieldParser "hgt" = Hgt <$> hgtParser
    fieldParser "hcl" = Hcl <$> hclParser
    fieldParser "ecl" = Ecl <$> eclParser
    fieldParser "pid" = Pid <$> pidParser
    fieldParser "cid" = Cid <$> takeWhile (not . isSpace)
    fieldParser f = fail $ "Invalid field: " ++ show f

    byrParser :: Parser Int
    byrParser = do
        val <- decimal
        guard $ val >= 1920 && val <= 2020
        return val

    iyrParser :: Parser Int
    iyrParser = do
        val <- decimal
        guard $ val >= 2010 && val <= 2020
        return val

    eyrParser :: Parser Int
    eyrParser = do
        val <- decimal
        guard $ val >= 2020 && val <= 2030
        return val

    hgtParser :: Parser Height
    hgtParser = do
        val <- decimal
        hgtCmParser val <|> hgtInParser val

    hgtCmParser x = string "cm" >> guard (x >= 150 && x <= 193) >> return (HeightCm x)
    hgtInParser x = string "in" >> guard (x >=  59 && x <=  76) >> return (HeightIn x)

    hclParser :: Parser RGB
    hclParser = do
        void $ char '#'
        [r1, r2, g1, g2, b1, b2] <- count 6 $ satisfy $ inClass "0-9a-f"
        r <- readHex' [r1, r2]
        g <- readHex' [g1, g2]
        b <- readHex' [b1, b2]
        return $ RGB r g b

    readHex' x = case readHex x of
                   [(val, "")] -> return val
                   _ -> fail $ "Invalid hexadecimal input: " ++ x

    eclParser :: Parser EyeColor
    eclParser = (string "amb" >> return Amber) <|>
                (string "blu" >> return Blue)  <|>
                (string "brn" >> return Brown) <|>
                (string "gry" >> return Gray)  <|>
                (string "grn" >> return Green) <|>
                (string "hzl" >> return Hazel) <|>
                (string "oth" >> return Other)

    pidParser :: Parser Int
    pidParser = count 9 digit >>= readOrFail

readOrFail :: (MonadFail m, Read a) => String -> m a
readOrFail input = maybe (fail "Invalid data") return $ readMaybe input

passportParser :: Parser Passport
passportParser = (foldl parseFields emptyBuilder <$> passportFieldParser `sepBy1` takeWhile1 isSpace) >>= finalize
 where
    emptyBuilder = PassportBuilder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    parseFields p (Byr x) = p { birthYear' = Just x }
    parseFields p (Iyr x) = p { issueYear' = Just x }
    parseFields p (Eyr x) = p { expirationYear' = Just x }
    parseFields p (Hgt x) = p { height' = Just x }
    parseFields p (Hcl x) = p { hairColor' = Just x }
    parseFields p (Ecl x) = p { eyeColor' = Just x }
    parseFields p (Pid x) = p { passportId' = Just x }
    parseFields p (Cid x) = p { countryId' = Just x }

    finalize (PassportBuilder (Just byr) (Just iyr) (Just eyr) (Just hgt) (Just hcl) (Just ecl) (Just pid) cid) =
        return $ Passport byr iyr eyr hgt hcl ecl pid cid
    finalize _ = fail "Incomplete passport"

main :: IO ()
main = do
    inputData <- TL.splitOn "\n\n" <$> TLIO.getContents
    print $ length $ filter isJust $  map (maybeResult . parse passportParser) inputData
