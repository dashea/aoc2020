import           Control.Monad (void)
import           Data.Attoparsec.Text.Lazy (Parser, anyChar, char, decimal, eitherResult, parse, skipSpace, takeLazyText)
import           Data.List.Safe ((!!))
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO

import Prelude hiding ((!!))

type Policy = ((Int, Int), Char)

main :: IO ()
main = do
    inputData <- (T.lines <$> TIO.getContents) >>=
        mapM (either fail return . eitherResult . parse inputParser)
    print $ length $ filter (uncurry isValid) inputData

-- isValidPart1 :: Policy -> Text -> Bool
-- isValidPart1 ((lower, upper), testChar) password =
--     let testCount = T.length $ T.filter (== testChar) password
--      in (testCount >= fromIntegral lower) && (testCount <= fromIntegral upper)

isValid :: Policy -> Text -> Bool
isValid ((idx1, idx2), testChar) password =
    let inputStr = T.unpack password
        c1 = inputStr !! (idx1 - 1)
        c2 = inputStr !! (idx2 - 1)
     in case (c1 == Just testChar, c2 == Just testChar) of
          (True, True)   -> False
          (False, False) -> False
          _              -> True

policyParser :: Parser Policy
policyParser = do
    lower <- decimal
    void $ char '-'
    upper <- decimal
    skipSpace
    testChar <- anyChar
    return ((lower, upper), testChar)

inputParser :: Parser (Policy, Text)
inputParser = do
    policy <- policyParser
    skipSpace
    void $ char ':'
    skipSpace
    password <- takeLazyText
    return (policy, password)
