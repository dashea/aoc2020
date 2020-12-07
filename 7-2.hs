{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Attoparsec.Text.Lazy
import           Data.Functor (($>))
import qualified Data.Graph.Inductive as G
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

import Prelude hiding (takeWhile)

type BagData = (String, [(Int, String)])

main :: IO ()
main = do
    inputData <- TL.lines <$> TLIO.getContents
    bags <- maybe (fail "Invalid bag data") return $ mapM (maybeResult . parse bagParser) inputData
    let edges = [(from, to, weight) | (to, froms)  <- bags,
                                      (weight, from) <- froms]
        empty = G.empty :: G.Gr String Int
        (shinyGold, (_, bagGraph)) = G.run empty $
                                     G.insMapNodeM "shiny gold" <*
                                     G.insMapNodesM (map fst bags) <*
                                     mapM_ G.insMapEdgeM edges

    -- subtract one for the path from the node to itself
    -- part 1
    -- print $ length (G.spTree (fst shinyGold) bagGraph) - 1

    -- subtract one since the answer should not count the gold bag itself
    (subtract 1 <$> walkDeps bagGraph (fst shinyGold)) >>= print

walkDeps :: (G.Graph gr, MonadFail m) => gr String Int -> G.Node -> m Int
walkDeps graph node =
    case G.match node graph of
      (Nothing, _) -> fail "malformed graph"
      (Just ctx, graph') ->
          -- add one for the bag being visited
          (1+) . sum <$> mapM (\(node', weight) -> (weight*) <$> walkDeps graph' node') (G.lpre' ctx)

bagParser :: Parser BagData
bagParser = do
    bagType <- anyChar `manyTill` (many1 space *> string "bags contain")
    skipSpace
    bagDeps <- (depParser `sepBy1` char ',') <|> (string "no other bags" $> [])
    void $ char '.'
    return (bagType, bagDeps)
 where
    depParser :: Parser (Int, String)
    depParser = do
        skipSpace
        depNum <- decimal
        skipSpace
        depDesc <- anyChar `manyTill` (skipSpace *> (string "bags" <|> string "bag"))
        return (depNum, depDesc)
