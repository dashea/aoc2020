{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (void)
import           Data.Attoparsec.Text (Parser, char, decimal, sepBy, sepBy1, skipSpace, string, takeWhile1)
import           Data.Conduit (ConduitT, (.|), ($$+), ($$++), await, runConduit, unsealConduitT, yield)
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit.Attoparsec (conduitParser, sinkParser)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)

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

ticketValueParser :: Parser [Int]
ticketValueParser = decimal `sepBy` char ','

sortTicketFields :: MonadFail m => [TicketField] -> ConduitT [Int] Void m [TicketField]
sortTicketFields fields =
    sortTicketFields' $ replicate (length fields) fields
 where
    sortTicketFields' :: MonadFail m => [[TicketField]] -> ConduitT [Int] Void m [TicketField]
    sortTicketFields' possibleFields = await >>= \case
        -- If every possible slot has exactly one ticket field, we're done, otherwise the data was bad
        Nothing -> mapM singletonOrFail possibleFields
        Just fields' -> sortTicketFields' $ reducePossibilities $ zipWith applyPossibilities fields' possibleFields

    singletonOrFail :: MonadFail m => [a] -> m a
    singletonOrFail [x] = return x
    singletonOrFail [] = fail "Bad data: field not possible"
    singletonOrFail _  = fail "Bad data: too many possibilities"

    applyPossibilities :: Int -> [TicketField] -> [TicketField]
    applyPossibilities val = filter (\(_, ranges) -> any ($ val) ranges)

    reducePossibilities :: [[TicketField]] -> [[TicketField]]
    reducePossibilities possibilities =
        let nextPossibilities = filterSingletons $ takeOnlys possibilities
         in if comparePossibilities possibilities nextPossibilities
              then nextPossibilities
              else reducePossibilities nextPossibilities

    comparePossibilities :: [[TicketField]] -> [[TicketField]] -> Bool
    comparePossibilities a b = map (map fst) a == map (map fst) b

    -- if any field exists in only one slot, that slot is that field
    takeOnlys :: [[TicketField]] -> [[TicketField]]
    takeOnlys possibilities =
        let onlys = filter (isOnly possibilities) fields
         in foldl (\acc o -> map (takeOnly o) acc) possibilities onlys
     where
        takeOnly :: TicketField -> [TicketField] -> [TicketField]
        takeOnly only ps =
            if fst only `elem` map fst ps then [only] else ps

    isOnly :: [[TicketField]] -> TicketField -> Bool
    isOnly possibilities field =
        let fieldNames = map (map fst) possibilities
         in isOnly' False fieldNames (fst field)
     where
        isOnly' seen (x:xs) f =
            if f `elem` x
              then not seen && isOnly' True xs f
              else isOnly' seen xs f
        isOnly' _ [] _ = True

    -- if any slot is down to one possibility, remove the possibility everywhere else
    filterSingletons :: [[TicketField]] -> [[TicketField]]
    filterSingletons possibilities =
        let singletons = mapMaybe singletonOrFail possibilities
         in foldl (\acc s -> map (filterOut s) acc) possibilities singletons

    filterOut :: TicketField -> [TicketField] -> [TicketField]
    filterOut x l@(_:_:_) = filter ((/= fst x) . fst) l
    filterOut _ l = l

main :: IO ()
main = do
    -- parse the field restrictions
    (remainder1, ticketFields) <- CC.stdin .|
                                  CC.decodeUtf8 $$+
                                  sinkParser (ticketFieldParser `sepBy1` char '\n')

    -- skip over the "your ticket:" header and grab the data
    (remainder2, yourTicket) <- remainder1 $$++
                                sinkParser (string "\n\nyour ticket:\n" >> ticketValueParser)

    -- skip over the "nearby tickets" header
    (remainder3, _) <- remainder2 $$++ sinkParser (string "\n\nnearby tickets:\n")

    let ticketRanges = concatMap snd ticketFields
    orderedFields <- runConduit $ unsealConduitT remainder3
                                .| conduitParser (ticketValueParser <* char '\n')
                                .| (yield yourTicket >> CC.map snd) -- discard positions, inject "yourTicket"
                                .| CC.filter (all (\x -> any ($ x) ticketRanges))
                                .| sortTicketFields ticketFields

    let orderedValues = zip (map fst orderedFields) yourTicket
        departureValues = map snd $ filter (("departure" `T.isPrefixOf`) . fst) orderedValues

    print $ product departureValues
