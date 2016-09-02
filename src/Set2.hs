{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Some a | None

instance Show a => Show (Maybe a) where
    show (Some a) = show a
    show None = "None"

headMay :: [a] -> Maybe a
headMay [] = None
headMay (x:xs) = Some x

tailMay :: [a] -> Maybe a
tailMay [] = None
tailMay (x:[]) = Some x
tailMay (x:xs) = tailMay xs

lookupMay :: Eq a => [(a, b)] -> a -> Maybe b
lookupMay ls x = case firstMatch of Some (key, value) -> Some value
                                    _ -> None
                 where firstMatch = headMay $ filter (\(y, z) -> y == x) ls

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x 0 = None
divMay x y = Some (x/y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = None
maximumMay (x:xs) = Some (foldl (\max a -> bigger a max) x xs)
  where
    bigger y z | y >= z = y
               | y < z = z

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = None
minimumMay (x:xs) = Some (foldl (\min a -> smaller a min) x xs)
  where
    smaller y z | y <= z = y
                | y > z = z
