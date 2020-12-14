{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import           Control.Applicative ((<|>))
import           Control.Monad (foldM, guard, void)
import           Control.Monad.State (State, evalState, get, gets, modify, put)
import           Data.Attoparsec.Text.Lazy
import           Data.Bits
import qualified Data.HashSet as HashSet
import           Data.HashSet (HashSet)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Word (Word64)

type BitMasks = (Word64, [Int])

data MaskBit = MaskNone | MaskOne | MaskFloat

data Instruction = UpdateBitmask BitMasks
                 | WriteMemory Word64 Word64
 deriving (Show)

instructionParser :: Parser Instruction
instructionParser =
    parseMask <|> parseWrite
 where
    parseMask :: Parser Instruction
    parseMask = do
        void $ string "mask"
        equals
        bits <- count 36 parseMaskBit
        let oneMask  = foldl bitsToOneMask 0 bits
            floatingBits = foldl bitsToFloating [] $ zip bits $ reverse [0..35]
        return $ UpdateBitmask (oneMask, floatingBits)

    equals :: Parser ()
    equals = skipSpace >> char '=' >> skipSpace

    -- fold into a mask to be ORed with a value to write, to set 1's
    bitsToOneMask :: Word64 -> MaskBit -> Word64
    bitsToOneMask acc MaskOne = (acc `shiftL` 1) .|. 1
    bitsToOneMask acc _       = (acc `shiftL` 1) .|. 0

    -- fold into a list of floating bit positions
    bitsToFloating :: [Int] -> (MaskBit, Int) -> [Int]
    bitsToFloating acc (MaskFloat, i) = i:acc
    bitsToFloating acc _ = acc

    parseMaskBit :: Parser MaskBit
    parseMaskBit = (char '0' >> return MaskNone) <|>
                   (char '1' >> return MaskOne)  <|>
                   (char 'X' >> return MaskFloat)

    parseWrite :: Parser Instruction
    parseWrite = do
        void $ string "mem"
        void $ char '['
        addr <- decimal
        void $ char ']'
        equals
        value <- decimal

        validateNumber addr
        validateNumber value

        return $ WriteMemory addr value

    validateNumber i = guard (i >= 0 && i < (2 ^ (36 :: Word64)))

parseProgram :: MonadFail m => TL.Text -> m [Instruction]
parseProgram input =
    either (\e -> fail $ "Invalid input: " ++ show e) return $ mapM (eitherResult . parse instructionParser) $ TL.lines input

allCombos :: [a] -> [[a]]
allCombos (x:xs) =
    let nextCombos = allCombos xs
     in map (x:) nextCombos ++ nextCombos
allCombos [] = [[]]

runProgram :: [Instruction] -> Word64
runProgram input =
    let writes = evalState (concat <$> mapM runInstruction input) (0, [])
     in evalState (foldM sumWrites 0 (reverse writes)) HashSet.empty
 where
    runInstruction :: Instruction -> State BitMasks [(Word64, Word64)]
    runInstruction (UpdateBitmask mask) = put mask >> return []
    runInstruction (WriteMemory addr val) = gets $ map (,val) . applyMask addr

    sumWrites :: Word64 -> (Word64, Word64) -> State (HashSet Word64) Word64
    sumWrites acc (addr, val) = do
        addrsUsed <- get
        let acc' = if HashSet.member addr addrsUsed then acc else acc + val
        modify $ HashSet.insert addr
        return acc'

    applyMask :: Word64 -> BitMasks -> [Word64]
    applyMask i (oneMask, floatBits) =
        let ones = i .|. oneMask
            floatsZero = foldl clearBit ones floatBits
            setAllBits = foldl setBit
         in map (setAllBits floatsZero) (allCombos floatBits)

main :: IO ()
main = do
    program <- TLIO.getContents >>= parseProgram
    print $ runProgram program
