{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
import           Control.Monad (foldM, guard, void)
import           Control.Monad.State (State, evalState, get, modify, put)
import           Data.Attoparsec.Text.Lazy
import           Data.Bits
import qualified Data.HashSet as HashSet
import           Data.HashSet (HashSet)
import           Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Word (Word64)

type BitMasks = (Word64, Word64)

data MaskBit = MaskZero | MaskOne | MaskNull

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
        let zeroMask = foldl bitsToZeroMask (complement 0) bits
            oneMask  = foldl bitsToOneMask 0 bits
        return $ UpdateBitmask (zeroMask, oneMask)

    equals :: Parser ()
    equals = skipSpace >> char '=' >> skipSpace

    -- fold mask bits into a mask to be ANDed with a value to write, to set 0's
    bitsToZeroMask :: Word64 -> MaskBit -> Word64
    bitsToZeroMask acc MaskZero = (acc `shiftL` 1) .|. 0
    bitsToZeroMask acc _        = (acc `shiftL` 1) .|. 1

    -- fold into a mask to be ORed with a value to write, to set 1's
    bitsToOneMask :: Word64 -> MaskBit -> Word64
    bitsToOneMask acc MaskOne = (acc `shiftL` 1) .|. 1
    bitsToOneMask acc _       = (acc `shiftL` 1) .|. 0

    parseMaskBit :: Parser MaskBit
    parseMaskBit = (char '0' >> return MaskZero) <|>
                   (char '1' >> return MaskOne)  <|>
                   (char 'X' >> return MaskNull)

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

runProgram :: [Instruction] -> Word64
runProgram input =
    let writes = evalState (catMaybes <$> mapM runInstruction input) (complement 0, 0)
     in evalState (foldM sumWrites 0 (reverse writes)) HashSet.empty
 where
    runInstruction :: Instruction -> State BitMasks (Maybe (Word64, Word64))
    runInstruction (UpdateBitmask mask) = put mask >> return Nothing
    runInstruction (WriteMemory addr val) = do
        mask <- get
        return $ Just (addr, applyMask val mask)

    sumWrites :: Word64 -> (Word64, Word64) -> State (HashSet Word64) Word64
    sumWrites acc (addr, val) = do
        addrsUsed <- get
        let acc' = if HashSet.member addr addrsUsed then acc else acc + val
        modify $ HashSet.insert addr
        return acc'

    applyMask :: Word64 -> BitMasks -> Word64
    applyMask i (zeroMask, oneMask) = (i .&. zeroMask) .|. oneMask

main :: IO ()
main = do
    program <- TLIO.getContents >>= parseProgram
    print $ runProgram program
