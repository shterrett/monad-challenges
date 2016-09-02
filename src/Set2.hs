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

tailMay :: [a] -> Maybe [a]
tailMay [] = None
tailMay (_:xs) = Some xs

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

queryGreek :: GreekData -> String -> Maybe Double
queryGreek db key = case lookupMay db key of Some xs -> doMath xs
                                             None -> None
  where doMath ys = let h = headMay ys
                        t = tailMay ys
                    in case (h, t) of (Some h1, Some t1) -> divide h1 t1
                                      _ -> None
        divide h t = case maximumMay t of Some t1 -> divMay (fromInteger t1) (fromInteger h)
                                          None -> None

-- queryGreek greekDataA "alpha" == Some 2.0
-- queryGreek greekDataA "beta" == None
-- queryGreek greekDataA "gamma" == Some 3.3333333333333335
-- queryGreek greekDataA "delta" == None
-- queryGreek greekDataA "zeta" == None
-- 
-- queryGreek greekDataB "rho" == None
-- queryGreek greekDataB "phi" == Some 0.24528301886792453
-- queryGreek greekDataB "chi" == Some 9.095238095238095
-- queryGreek greekDataB "psi" == None
-- queryGreek greekDataB "omega" == Some 24.0
