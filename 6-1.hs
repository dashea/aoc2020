{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative (Alternative)
import           Control.Monad (foldM, guard)
import           Data.Bits (popCount, setBit)
import           Data.Functor (($>))
import           Data.Char (isSpace, ord)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Word (Word32)

charToBit :: Alternative m => Char -> m Int
charToBit input =
    let bit = ord input - ord 'a'
     in guard ((bit >= 0) && (bit <= 26)) $> bit

answerCount :: (Alternative m, Monad m) => String -> m Int
answerCount input = popCount <$> foldM setAnswer (0 :: Word32) (filter (not . isSpace) input)
 where
    setAnswer acc c = (acc `setBit`) <$> charToBit c

main :: IO ()
main =
    ((map TL.unpack . TL.splitOn "\n\n" <$> TLIO.getContents) >>= mapM answerCount) >>=
        print . sum
