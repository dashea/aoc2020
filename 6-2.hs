{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative (Alternative)
import           Control.Monad (foldM, guard)
import           Control.Monad.Catch (MonadThrow)
import           Data.Bits ((.&.), popCount, setBit)
import           Data.Functor (($>))
import           Data.Char (ord)
import           Data.List.Safe (foldl1)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Word (Word32)

import Prelude hiding (foldl1)

charToBit :: Alternative m => Char -> m Int
charToBit input =
    let bit = ord input - ord 'a'
     in guard ((bit >= 0) && (bit <= 26)) $> bit

-- answerCount :: (Alternative m, Monad m) => String -> m Int
-- answerCount input = popCount <$> foldM setAnswer (0 :: Word32) (filter (not . isSpace) input)
--  where
--     setAnswer acc c = (acc `setBit`) <$> charToBit c

answerCount :: (MonadThrow m, Alternative m) => String -> m Int
answerCount input =
    popCount <$> (mapM (foldM setAnswer (0 :: Word32)) (lines input) >>= foldl1 (.&.))
 where
    setAnswer acc c = (acc `setBit`) <$> charToBit c

main :: IO ()
main =
    ((map TL.unpack . TL.splitOn "\n\n" <$> TLIO.getContents) >>= mapM answerCount) >>=
        print . sum
