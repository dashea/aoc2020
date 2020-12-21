{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad (foldM, void)
import           Control.Monad.State (State, runState, get, put)
import           Data.Functor (($>))
import           Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet
import           Data.HashSet (HashSet)
import qualified Data.HashMap.Lazy as HashMap
import           Data.HashMap.Lazy (HashMap)
import           Data.List (nub)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           GHC.Generics (Generic)
import           Text.Parsec hiding (State)
import           Text.Parsec.Text (Parser)

data Symbol = A | B
 deriving (Bounded, Enum, Eq, Generic, Show)

instance Hashable Symbol

data RuleWithReference = LiteralReference Symbol
                       | Reference Int
                       | SequenceReference RuleWithReference RuleWithReference
                       | ChoiceReference RuleWithReference RuleWithReference
 deriving (Show)

data Rule = Literal Symbol
          | Sequence Rule Rule
          | Choice Rule Rule
 deriving (Show)

data NFA q s = NFA
    { _start      :: q
    , _end        :: q
    , _transition :: q -> s -> [q]
    }

-- runNFA :: Eq q => NFA q s -> [s] -> Bool
-- runNFA (NFA i e t) = elem e . foldM t i

data DFA q s = DFA
    { _dfaStart      :: q
    , _dfaEnd        :: [q]
    , _dfaTransition :: q -> s -> Maybe q
    }

runDFA :: Eq q => DFA q s -> [s] -> Bool
runDFA (DFA i e t) = isEnd . foldM t i
 where
    isEnd (Just x) = x `elem` e
    isEnd Nothing = False

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

dereferenceRule :: HashMap Int RuleWithReference -> Int -> Maybe Rule
dereferenceRule hm ridx = HashMap.lookup ridx hm >>= dereference'
 where
    dereference' :: RuleWithReference -> Maybe Rule
    dereference' (LiteralReference c) = return $ Literal c
    dereference' (SequenceReference a b) = do
        aRef <- dereference' a
        bRef <- dereference' b
        return $ Sequence aRef bRef
    dereference' (ChoiceReference a b) = do
        aRef <- dereference' a
        bRef <- dereference' b
        return $ Choice aRef bRef
    dereference' (Reference i) = dereferenceRule hm i

ruleToNFA :: Rule -> NFA Int Symbol
ruleToNFA rule =
    let startState = 0
        (stateMap, endState) = runState (ruleToNFA' HashMap.empty rule) startState
     in NFA startState endState (transition stateMap)
 where
    transition :: HashMap (Int, Maybe Symbol) [Int] -> Int -> Symbol -> [Int]
    transition stateMap q c =
        let possibleStarts = q : applyNulls stateMap [q]
            possibleNexts = concatMap (\q' -> HashMap.lookupDefault [] (q', Just c) stateMap) possibleStarts
            possibleEnds = applyNulls stateMap possibleNexts
         in nub possibleEnds

    applyNulls :: HashMap (Int, Maybe Symbol) [Int] -> [Int] -> [Int]
    applyNulls stateMap input = input >>= \i -> do
        let nextStates = HashMap.lookupDefault [] (i, Nothing) stateMap
        i : nextStates ++ applyNulls stateMap nextStates

    ruleToNFA' :: HashMap (Int, Maybe Symbol) [Int] -> Rule -> State Int (HashMap (Int, Maybe Symbol) [Int])
    ruleToNFA' stateMap (Literal c) = do
        startState <- get
        let endState = startState + 1
            nextMap = HashMap.insert (startState, Just c) [endState] stateMap
        put endState
        return nextMap

    -- use the same state as the end of a and the start of b to connect the sequence
    ruleToNFA' stateMap (Sequence a b) = do
        aMap <- ruleToNFA' stateMap a
        ruleToNFA' aMap b

    -- create a null transition from the current state to the start state of
    -- each choice, and then a null transition from the end of each choice to a new end
    ruleToNFA' stateMap (Choice a b) = do
        startState <- get

        let startA = startState + 1
            (mapA, endA) = runState (ruleToNFA' stateMap a) startA
            startB = endA + 1
            (mapB, endB) = runState (ruleToNFA' mapA b) startB
            finalEnd = endB + 1
            mapToStarts = HashMap.insert (startState, Nothing) [startA, startB] mapB
            mapFromEndA = HashMap.insert (endA, Nothing) [finalEnd] mapToStarts
            mapFromEndB = HashMap.insert (endB, Nothing) [finalEnd] mapFromEndA

        put finalEnd
        return mapFromEndB

nfaToDfa :: forall q s . (Eq q, Hashable q, Bounded s, Enum s, Eq s, Hashable s)
         => NFA q s
         -> DFA (HashSet q) s
nfaToDfa (NFA i e t) =
    let dfaStart = HashSet.singleton i
        mapping = makeMapping dfaStart HashMap.empty
        endStates = findEnds mapping
     in DFA dfaStart endStates (transition mapping)
 where
    transition :: HashMap (HashSet q, s) (HashSet q) -> HashSet q -> s -> Maybe (HashSet q)
    transition stateMap state c = HashMap.lookup (state, c) stateMap

    -- any of the combined DFA states that contains the original NFA end is a DFA end
    findEnds :: HashMap (HashSet q, s) (HashSet q) -> [HashSet q]
    findEnds mapping =
        let endMap = HashMap.filterWithKey (\_ v -> HashSet.member e v) mapping
         in HashMap.elems endMap

    makeMapping :: HashSet q -> HashMap (HashSet q, s) (HashSet q) -> HashMap (HashSet q, s) (HashSet q)
    makeMapping dfaState mapping = foldl (makeOneMapping dfaState) mapping [minBound..maxBound]

    makeOneMapping :: HashSet q -> HashMap (HashSet q, s) (HashSet q) -> s -> HashMap (HashSet q, s) (HashSet q)
    makeOneMapping dfaState mapping input =
        let dfaState' = HashSet.unions $ map (HashSet.fromList . (`t` input)) $ HashSet.toList dfaState
            mapping' = HashMap.insert (dfaState, input) dfaState' mapping
         in if HashMap.member (dfaState, input) mapping
              then mapping
              else makeMapping dfaState' mapping'

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
    let ruleMap = foldl (\acc (idx, r) -> HashMap.insert idx r acc) HashMap.empty parsedRules

    rule0 <- maybe (fail "Bad input") return $ dereferenceRule ruleMap 0

    let nfa = ruleToNFA rule0
        dfa = nfaToDfa nfa

    inputData <- mapM parseData $ T.lines text

    print $ length $ filter (runDFA dfa) inputData
