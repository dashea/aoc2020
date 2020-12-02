import           Control.Monad (void)
import           Data.Attoparsec.Text (Parser, anyChar, char, decimal, parseOnly, skipSpace, takeText)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO

type Policy = ((Int, Int), Char)

main :: IO ()
main = do
    inputData <- (T.lines <$> TIO.getContents) >>=
        mapM (either fail return . parseOnly inputParser)
    print $ length $ filter (uncurry isValid) inputData

isValid :: Policy -> Text -> Bool
isValid ((lower, upper), testChar) input =
    let testCount = T.length $ T.filter (== testChar) input
     in (testCount >= lower) && (testCount <= upper)

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
    password <- takeText
    return (policy, password)
