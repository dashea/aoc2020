{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (foldM)
import qualified Data.HashMap.Lazy as HashMap
import           Data.HashMap.Lazy (HashMap)
import           Data.List ((\\), intersect, nub)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO

updateAllergens :: MonadFail m
                => HashMap String [String]
                -> ([String], [String])
                -> m (HashMap String [String])
updateAllergens initialMap (ingredients, allergens) =
    foldM updateAllergen initialMap allergens
 where
    updateAllergen :: MonadFail m
                   => HashMap String [String]
                   -> String
                   -> m (HashMap String [String])
    updateAllergen allergenToIngredients allergen =
        case HashMap.lookup allergen allergenToIngredients of
          -- first instance of allergen, could be present in any ingredient
          Nothing -> return $ HashMap.insert allergen ingredients allergenToIngredients

          -- already some data: the possible ingredients containing the allergen is the intersection of the
          -- two lists. If the intersection is empty, bomb out
          Just is -> case ingredients `intersect` is of
                       [] -> fail $ "No possible ingredient for allergen " ++ allergen
                       inter -> return $ HashMap.insert allergen inter allergenToIngredients

parseIngredientList :: MonadFail m => Text -> m ([String], [String])
parseIngredientList input = parseIngredientList' [] (T.words input)
 where
    parseIngredientList' acc ("(contains" : xs) = return (acc, parseAllergens [] xs)
    parseIngredientList' acc (x : xs) = parseIngredientList' (T.unpack x : acc) xs
    parseIngredientList' _ [] = fail "Missing allergens"

    parseAllergens acc [] = acc
    parseAllergens acc (x : xs) =
        let allergen = if ("," `T.isSuffixOf` x) || (")" `T.isSuffixOf` x) then T.dropEnd 1 x else x
         in parseAllergens (T.unpack allergen : acc) xs

main :: IO ()
main = do
    input <- T.lines <$> TIO.getContents
    ingredientList <- mapM parseIngredientList input
    allergenMap <- foldM updateAllergens HashMap.empty ingredientList

    let allIngredients = concatMap fst ingredientList
        nubIngredients = nub allIngredients
        allAllergenIngredients = nub $ concat $ HashMap.elems allergenMap
        notAllergenIngredients = nubIngredients \\ allAllergenIngredients
        notAllergenCounts = map (\x -> length $ filter (== x) allIngredients) notAllergenIngredients
    print $ sum notAllergenCounts
