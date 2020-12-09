{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative((<|>))
import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Data.Attoparsec.Text.Lazy
import           Data.Functor (($>))
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Vector (Vector, (!?), fromList)
import           Type.Reflection (Typeable)

data AoCException = ComputerError String
                  | ParseError String
 deriving (Show, Typeable)

instance Exception AoCException

data OpCode = Noop Int
            | Acc Int
            | Jmp Int

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
    return (0, 0, fromList opCodes)

findLoop :: MonadThrow m => IntSet -> Computer -> m Int
findLoop is (ip, acc, opCodes) = do
    currentCode <- maybe (throwM $ ComputerError "Read past end of program") return $ opCodes !? ip
    let (nextAcc, nextIp) = case currentCode of
                              Noop _ -> (acc, ip + 1)
                              Acc i  -> (acc + i, ip + 1)
                              Jmp i  -> (acc, ip + i)
        nextIs = IntSet.insert ip is

    if IntSet.member nextIp is
      then return acc
      else findLoop nextIs (nextIp, nextAcc, opCodes)

main :: IO ()
main = TLIO.getContents >>= parseComputer >>= findLoop IntSet.empty >>= print
