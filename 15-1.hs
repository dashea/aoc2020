import           Data.Conduit (ConduitT, (.|), await, runConduitPure, yield)
import qualified Data.Conduit.Combinators as CC
import qualified Data.HashMap.Lazy as HashMap
import           Data.HashMap.Lazy (HashMap)

takeTurn :: Monad m => Int -> Int -> HashMap Int Int -> HashMap Int Int -> ConduitT Int Int m ()
takeTurn prevTurn prevInput prevMap curMap = do
    input <- await
    let turnNum = prevTurn + 1
        output = case input of
                   Just i -> i
                   Nothing -> case HashMap.lookup prevInput prevMap of
                                Nothing -> 0
                                Just lastTurn -> prevTurn - lastTurn
    yield output
    takeTurn turnNum output curMap $ HashMap.insert output turnNum curMap

main :: IO ()
main = do
    let startingNumbers = mapM_ yield [0,20,7,16,1,18,15]
        pipeline = startingNumbers .|
                   takeTurn 0 0 HashMap.empty HashMap.empty .|
                   CC.take 2020 .|
                   (CC.drop 2019 >> CC.head)
    maybe (fail "Error getting answer") return (runConduitPure pipeline) >>= print
