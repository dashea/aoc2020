import Data.List (tails)
main :: IO ()
main = do
    input <- map read . lines <$> getContents
    let products = [x*y | (x:ys) <- tails input,
                          y <- ys,
                          (x + y) == 2020]
    print $ head products
