import Data.List (tails)
main :: IO ()
main = do
    input <- map read . lines <$> getContents
    let products = [x*y*z | (x:ys) <- tails input,
                            (y:zs) <- tails ys,
                            z <- zs,
                            (x + y + z) == 2020]
    print $ head products
