{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad (void)
import           Data.Functor (($>))
import qualified Data.HashMap.Lazy as HashMap
import           Data.HashMap.Lazy (HashMap)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec hiding (State)
import           Text.Parsec.Text (Parser)

data Symbol = A | B
 deriving (Eq, Show)

data RuleWithReference = LiteralReference Symbol
                       | Reference Int
                       | SequenceReference RuleWithReference RuleWithReference
                       | ChoiceReference RuleWithReference RuleWithReference
 deriving (Show)

matchInput :: HashMap Int RuleWithReference -> Int -> [Symbol] -> Bool
matchInput mapping current input =
    case matchInput' current input of
      Just x -> [] `elem` x
      Nothing -> False
 where
    matchInput' current' input' = do
        rule <- HashMap.lookup current' mapping
        matchRule rule input'

    matchRule _ [] = Nothing

    matchRule (LiteralReference x) (c:cs)
      | x == c = return [cs]
      | otherwise = Nothing

    matchRule (Reference i) input' = matchInput' i input'

    matchRule (SequenceReference a b) input' = do
        remaindersA <- matchRule a input'
        case mapMaybe (matchRule b) remaindersA of
          [] -> Nothing
          x  -> Just $ concat x

    matchRule (ChoiceReference a b) input' = do
        let tryA = matchRule a input'
            tryB = matchRule b input'
        case (tryA, tryB) of
          (Just a', Just b') -> Just (a' ++ b')
          (Just a', Nothing) -> Just a'
          (Nothing, Just b') -> Just b'
          (Nothing, Nothing) -> Nothing


ruleParser :: Parser (Int, RuleWithReference)
ruleParser = do
    ruleNum <- read <$> many1 digit
    void $ char ':'
    void $ char ' '

    rule <- literal <|> choices

    return (ruleNum, rule)

 where
    decimal = Reference <$> (spaces >> ((read <$> many1 digit) <* spaces))

    literal = LiteralReference <$>
        between (char '"') (char '"') ((char 'a' $> A) <|> (char 'b' $> B))

    refSequence =
        decimal `chainl1` (spaces >> return SequenceReference)

    choices =
        refSequence `chainl1` (char '|' >> return ChoiceReference)

parseData :: MonadFail m => Text -> m [Symbol]
parseData input = mapM toSymbol $ T.unpack input
 where
    toSymbol 'a' = return A
    toSymbol 'b' = return B
    toSymbol _   = fail "Invalid input"

main :: IO ()
main = do
    [rules, text] <- T.splitOn "\n\n" <$> TIO.getContents
    parsedRules <- either (fail . show) return $ mapM (parse ruleParser "input") $ T.lines rules
    -- modify rules for part 2
    let ruleMap = foldl (\acc (idx, r) -> HashMap.insert idx r acc) HashMap.empty parsedRules
        ruleMap8 = HashMap.insert 8 (ChoiceReference (Reference 42) (SequenceReference (Reference 42) (Reference 8))) ruleMap
        ruleMap11 = HashMap.insert 11 (ChoiceReference (SequenceReference (Reference 42) (Reference 31)) (SequenceReference (SequenceReference (Reference 42) (Reference 11)) (Reference 31))) ruleMap8

    inputData <- mapM parseData $ T.lines text
    print $ length $ filter (matchInput ruleMap11 0) inputData
