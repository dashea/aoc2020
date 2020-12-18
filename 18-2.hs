{-# LANGUAGE OverloadedStrings #-}

import           Data.Char (isSpace)
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Lazy as TL
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Text (Parser)

data Expression = Plus Expression Expression
                | Times Expression Expression
                | Constant Integer
 deriving (Show)

expressionParser :: Parser Expression
expressionParser = expr
 where
    skipSpace = skipMany $ satisfy (\c -> isSpace c && (c /= '\n'))

    parens = between (char '(') (char ')') expr

    term = skipSpace >> ((parens <|> constant) <* skipSpace)

    expr = buildExpressionParser operators term

    operators = [ [ Infix (char '+' >> return Plus) AssocLeft ]
                , [ Infix (char '*' >> return Times)  AssocLeft ] ]

    constant = Constant . read <$> many1 digit

calculator :: Expression -> Integer
calculator (Constant i) = i
calculator (Plus l r) = calculator l + calculator r
calculator (Times l r) = calculator l * calculator r

main :: IO ()
main = do
    input <- map TL.toStrict . TL.lines <$> TLIO.getContents
    (sum . map calculator <$> either (fail . show) return (mapM (parse expressionParser "input") input)) >>= print
