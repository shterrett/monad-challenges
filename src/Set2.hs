{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Some a | None
             deriving Eq

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

-- queryGreek2 greekDataA "alpha" == Some 2.0
-- queryGreek2 greekDataA "beta" == None
-- queryGreek2 greekDataA "gamma" == Some 3.3333333333333335
-- queryGreek2 greekDataA "delta" == None
-- queryGreek2 greekDataA "zeta" == None
-- 
-- queryGreek2 greekDataB "rho" == None
-- queryGreek2 greekDataB "phi" == Some 0.24528301886792453
-- queryGreek2 greekDataB "chi" == Some 9.095238095238095
-- queryGreek2 greekDataB "psi" == None
-- queryGreek2 greekDataB "omega" == Some 24.0

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ None = None
chain f (Some x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link None _ = None
link (Some x) f = f x

(<&>) :: Maybe a -> (a -> Maybe b) -> Maybe b
(<&>) = link

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 db key = (lookupMay db key) <&> divHeadTail
  where divHeadTail xs = let denom = headMay xs
                             num = tailMay xs <&> maximumMay
                         in num <&> (\n -> denom <&>
                                           (\d -> Some(fromInteger d))  <&>
                                                  divMay (fromInteger n))

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries db n1 n2 = let v1 = lookup db n1
                           v2 = lookup db n2
                       in case (v1, v2) of
                            (Some p1, Some p2) -> mkMaybe ((snd p1) + (snd p2))
                            _ -> None
    where lookup [] _ = None
          lookup xs y = case xs of
                          [] -> None
                          (x:xs) -> if (fst x) == y
                                    then mkMaybe x
                                    else
                                      lookup xs y

yLink ::  (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f (Some x) (Some y) = mkMaybe (f x y)
yLink _ _ _ = None

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 db n1 n2 = yLink (\x y -> (snd x) + (snd y)) (lookup db n1) (lookup db n2)
    where lookup [] _ = None
          lookup xs y = case xs of
                          [] -> None
                          (x:xs) -> if (fst x) == y
                                    then mkMaybe x
                                    else
                                      lookup xs y

mkMaybe :: a -> Maybe a
mkMaybe a = Some a

tailProd :: Num a => [a] -> Maybe a
tailProd = chain (\xs -> mkMaybe (product xs)) . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = chain (\xs -> mkMaybe (sum xs)) . tailMay

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f (Some xs) = mkMaybe $ f xs
transMaybe _ _ = None

tailProd1 :: Num a => [a] -> Maybe a
tailProd1 = transMaybe (\xs -> product xs) . tailMay

tailSum1 :: Num a => [a] -> Maybe a
tailSum1 = transMaybe (\xs -> sum xs) . tailMay

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = link (tailMay xs) maximumMay

tailMin :: Ord a => [a] -> Maybe a
tailMin xs = link (tailMay xs) minimumMay

tailMaxTrans :: Ord a => [a] -> Maybe (Maybe a)
tailMaxTrans = transMaybe maximumMay . tailMay

tailMinTrans :: Ord a => [a] -> Maybe (Maybe a)
tailMinTrans = transMaybe minimumMay . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine (Some (Some x)) = Some x
combine _ = None

tailMaxTransFlat :: Ord a => [a] -> Maybe a
tailMaxTransFlat = combine . transMaybe maximumMay . tailMay

tailMinTransFlat :: Ord a => [a] -> Maybe a
tailMinTransFlat = combine . transMaybe maximumMay . tailMay

