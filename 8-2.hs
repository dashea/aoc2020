{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative((<|>))
import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Data.Attoparsec.Text.Lazy
import           Data.Functor (($>))
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as V
import           Type.Reflection (Typeable)

data AoCException = ComputerError String
                  | ParseError String
 deriving (Show, Typeable)

data ComputerStatus = Running | Halted

instance Exception AoCException

data OpCode = Noop Int
            | Acc Int
            | Jmp Int
 deriving (Show)

type Computer = (Int, Int, Vector OpCode)

parseOpCode :: Parser OpCode
parseOpCode = do
    base <- (string "nop" $> Noop) <|>
            (string "acc" $> Acc)  <|>
            (string "jmp" $> Jmp)
    skipSpace

    -- require the sign
    sign <- (string "+" $> id) <|> (string "-" $> negate)
    base . sign <$> decimal

parseComputer :: MonadThrow m => TL.Text -> m Computer
parseComputer input = do
    let eitherOpCodeList = mapM (eitherResult . parse parseOpCode) $ TL.lines input
    opCodes <- either (throwM . ParseError) return eitherOpCodeList
    return (0, 0, V.fromList opCodes)

findLoop :: IntSet -> Computer -> (ComputerStatus, Int)
findLoop is computer@(ip, acc, _) =
    let nextIs = IntSet.insert ip is
        (status, computer'@(nextIp, _, _)) = runInstruction computer
     in case status of
          Halted -> (Halted, acc)
          Running -> if IntSet.member nextIp is
                       then (Running, acc)
                       else findLoop nextIs computer'

fixLoop :: MonadThrow m => Computer -> m Int
fixLoop (ip, acc, ins) = tryLoop rewrite
 where
    tryLoop :: MonadThrow m => [Vector OpCode] -> m Int
    tryLoop [] = throwM $ ComputerError "No cases halt"
    tryLoop (i:is) = case findLoop IntSet.empty (ip, acc, i) of
                       (Halted, acc') -> return acc'
                       (Running, _) -> tryLoop is

    rewrite :: [Vector OpCode]
    rewrite = mapMaybe rewriteOne [0..(V.length ins - 1)]

    rewriteOne :: Int -> Maybe (Vector OpCode)
    rewriteOne i = do
        newIns <- ins !? i >>= \case
            Noop x -> Just $ Jmp x
            Jmp x  -> Just $ Noop x
            Acc _  -> Nothing
        Just $ ins // [(i, newIns)]


runInstruction :: Computer -> (ComputerStatus, Computer)
runInstruction computer@(ip, acc, ins) =
    case ins !? ip of
      Just currentCode ->
          let (ip', acc') = case currentCode of
                              Noop _ -> (ip + 1, acc)
                              Acc i  -> (ip + 1, acc + i)
                              Jmp i  -> (ip + i, acc)
           in (Running, (ip', acc', ins))
      Nothing -> (Halted, computer)

main :: IO ()
-- main = TLIO.getContents >>= parseComputer >>= findLoop IntSet.empty >>= print
main = TLIO.getContents >>= parseComputer >>= fixLoop >>= print
