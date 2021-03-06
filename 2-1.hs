import           Control.Monad (void)
import           Data.Attoparsec.Text.Lazy (Parser, anyChar, char, decimal, eitherResult, parse, skipSpace, takeLazyText)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO

type Policy = ((Int, Int), Char)

main :: IO ()
main = do
    inputData <- (T.lines <$> TIO.getContents) >>=
        mapM (either fail return . eitherResult . parse inputParser)
    print $ length $ filter (uncurry isValid) inputData

isValid :: Policy -> Text -> Bool
isValid ((lower, upper), testChar) password =
    let testCount = T.length $ T.filter (== testChar) password
     in (testCount >= fromIntegral lower) && (testCount <= fromIntegral upper)

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
