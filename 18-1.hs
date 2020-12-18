{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Attoparsec.Text (Parser, char, decimal, endOfLine, option, skipWhile)
import           Data.Char (isSpace)
import           Data.Conduit (ConduitT, (.|), awaitForever, runConduit, yield)
import           Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.Combinators as CC
import           Data.Functor (($>))

data Expression = Plus Expression Expression
                | Times Expression Expression
                | Constant Integer
 deriving (Show)

expressionParser :: Parser Expression
expressionParser =
    expression <* endOfLine
 where
    expression = do
        lhs <- factor
        chainExpr lhs

    factor = constant <|> parens

    parens = do
        void $ char '('
        skipSpace'
        e <- expression
        skipSpace'
        void $ char ')'

        return e

    chainExpr lhs = option lhs $ do
        skipSpace'
        o <- (char '+' $> Plus) <|> (char '*' $> Times)
        skipSpace'
        rhs <- factor

        chainExpr $ o lhs rhs

    constant = Constant <$> decimal

    skipSpace' = skipWhile (\c -> isSpace c && (c /= '\n'))

calculator :: Expression -> Integer
calculator (Constant i) = i
calculator (Plus l r) = calculator l + calculator r
calculator (Times l r) = calculator l * calculator r

calculatorC :: Monad m => ConduitT Expression Integer m ()
calculatorC = awaitForever $ \e -> yield (calculator e)

main :: IO ()
main = do
    result <- runConduit $ CC.stdin
                        .| CC.decodeUtf8
                        .| conduitParser expressionParser
                        .| CC.map snd
                        .| calculatorC
                        .| CC.sum
    print result
