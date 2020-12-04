{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (foldM, void)
import           Data.Attoparsec.Combinator (sepBy1)
import           Data.Attoparsec.Text.Lazy (Parser, char, maybeResult, parse, takeWhile1)
import           Data.Char (isSpace)
import           Data.Maybe (isJust)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

data Passport =
  Passport
    { _birthYear :: Text
    , _issueYear :: Text
    , _expirationYear :: Text
    , _height :: Text
    , _hairColor :: Text
    , _eyeColor :: Text
    , _passportId :: Text
    , _countryId :: Maybe Text
    }

-- same as above but with everything optional, in order to build from a stream of pairs
data PassportBuilder =
  PassportBuilder
    { birthYear' :: Maybe Text
    , issueYear' :: Maybe Text
    , expirationYear' :: Maybe Text
    , height' :: Maybe Text
    , hairColor' :: Maybe Text
    , eyeColor' :: Maybe Text
    , passportId' :: Maybe Text
    , countryId' :: Maybe Text
    }

pairParser :: Parser (Text, Text)
pairParser = do
    field <- takeWhile1 (/= ':')
    void $ char ':'
    value <- takeWhile1 (not . isSpace)
    return (field, value)

passportParser :: Parser Passport
passportParser = pairParser `sepBy1` takeWhile1 isSpace >>= foldM parseFields emptyBuilder >>= finalize
 where
    emptyBuilder = PassportBuilder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    parseFields p ("byr", value) = return $ p { birthYear' = Just value }
    parseFields p ("iyr", value) = return $ p { issueYear' = Just value }
    parseFields p ("eyr", value) = return $ p { expirationYear' = Just value }
    parseFields p ("hgt", value) = return $ p { height' = Just value }
    parseFields p ("hcl", value) = return $ p { hairColor' = Just value }
    parseFields p ("ecl", value) = return $ p { eyeColor' = Just value }
    parseFields p ("pid", value) = return $ p { passportId' = Just value }
    parseFields p ("cid", value) = return $ p { countryId' = Just value }
    parseFields _ (field, _) = fail $ "Invalid field: " ++ T.unpack field

    finalize (PassportBuilder (Just byr) (Just iyr) (Just eyr) (Just hgt) (Just hcl) (Just ecl) (Just pid) cid) =
        return $ Passport byr iyr eyr hgt hcl ecl pid cid
    finalize _ = fail "Incomplete passport"

main :: IO ()
main = do
    inputData <- TL.splitOn "\n\n" <$> TLIO.getContents
    print $ length $ filter isJust $  map (maybeResult . parse passportParser) inputData
