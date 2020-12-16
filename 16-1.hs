{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (void)
import           Data.Attoparsec.Text (Parser, char, decimal, sepBy, sepBy1, skipSpace, skipWhile, string, takeWhile1)
import           Data.Conduit (ConduitT, (.|), ($$+), ($$++), awaitForever, runConduit, unsealConduitT, yield)
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit.Attoparsec (conduitParser, sinkParser)
import           Data.Text (Text)

type TicketRange = [Int -> Bool]
type TicketField = (Text, TicketRange)

ticketFieldParser :: Parser TicketField
ticketFieldParser = do
    fieldName <- takeWhile1 (\c -> c /= ':' && c /= '\n')
    void $ char ':'
    skipSpace
    rules <- rangeParser `sepBy1` (skipSpace >> string "or" >> skipSpace)

    return (fieldName, rules)
 where
    rangeParser :: Parser (Int -> Bool)
    rangeParser = do
        lower <- decimal
        void $ char '-'
        upper <- decimal

        return $ \x -> x >= lower && x <= upper

unconcat :: Monad m => ConduitT [a] a m ()
unconcat = awaitForever $ \x -> mapM_ yield x

main :: IO ()
main = do
    -- parse the field restrictions
    (remainder1, ticketFields) <- CC.stdin .|
                                  CC.decodeUtf8 $$+
                                  sinkParser (ticketFieldParser `sepBy1` char '\n')

    -- skip over the "your ticket:" data and "nearby tickets" header
    (remainder2, _) <- remainder1 $$++
                       sinkParser (string "\n\nyour ticket:\n" >>
                                   skipWhile (/= '\n') >>
                                   string "\n\nnearby tickets:\n")

    let ticketRanges = concatMap snd ticketFields
        pipeline = unsealConduitT remainder2 .|
                   conduitParser (((decimal :: Parser Int) `sepBy` char ',') <* char '\n') .|
                   CC.map snd .|    -- discard the positions
                   unconcat .|
                   CC.filter (\x -> not $ any ($ x) ticketRanges) .|
                   CC.sum

    runConduit pipeline >>= print
