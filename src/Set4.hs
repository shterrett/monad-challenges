{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude

data Maybe a = Some a | None
             deriving Eq

instance Show a => Show (Maybe a) where
    show (Some a) = show a
    show None = "None"

-- generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
-- yLink ::  (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- yComb :: (a -> b -> c) -> m a -> m b -> m c
--
-- generalA2 :: Gen a -> (a -> a) -> Gen a
-- transMaybe :: (a -> b) -> Maybe a -> Maybe b
-- pass :: (a -> b) -> m a -> m b
--
-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- through :: m a -> (a -> m b) -> m b
--
-- mkGen :: a -> Gen a
-- mkMaybe :: a -> Maybe a
-- make :: a -> m a

class Monad m where
    bind :: m a -> (a -> m b) -> m b
    return :: a -> m a
    yLink :: m a -> m b -> (a -> b -> c) -> m c
    yLink  m1 m2 f = bind m2 (\v2 -> bind mg (\g -> return (g v2)))
      where mg = bind m1 (\v1 -> return (f v1)) -- -> Monad (b -> c)

instance Monad Maybe where
    return a = Some a
    bind (Some x) f = f x
    bind None _ = None

instance Monad [] where
    return a = [a]
    bind (g:gs) f = (f g) ++ bind gs f
    bind [] f = []

newtype Gen a = Gen { runGen :: (Seed -> (a, Seed)) }

evalGen :: Gen a -> Seed -> a
evalGen (Gen g) s = fst (g s)

instance Monad Gen where
    return x = Gen (\s -> (x, s))
    bind ga f = Gen (\s ->
                      let (a, s2) = runGen ga s
                      in runGen (f a) s2)

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (x:xs) = yLink (toList x) (sequence xs) (\b c -> c ++ b)
  where toList m1 = bind m1 (\y -> return [y])

liftM :: Monad m => m a -> (a -> b) -> m b
liftM m f = bind m (\v -> return (f v))

liftM2 :: Monad m => m a -> m b -> (a -> b -> c) -> m c
liftM2 = yLink

liftM3 :: Monad m => m a -> m b -> m c ->  (a -> b -> c -> d) -> m d
liftM3 m1 m2 m3 f = bind m3 (\v3 -> bind mg (\g -> return (g v3)))
    where mg = liftM2 m1 m2 f -- Monad (c -> d)

join :: Monad m => m (m a) -> m a
join x = bind x id

chain :: Monad m => (a -> m b) -> m a -> m b
chain = flip bind

(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) = bind

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = liftM2 mf ma (\f va -> f va)

-- Set 1

fiveRands :: [Integer]
fiveRands = let rands = sequence $ take 5 $ cycle [Gen rand]
            in evalGen rands (mkSeed 1)

randLetter :: Gen Char
randLetter = liftM (Gen rand) toLetter

randString3 :: String
randString3 = let rands = sequence $ take 3 $ cycle [randLetter]
              in evalGen rands (mkSeed 1)

randEven :: Gen Integer
randEven = liftM (Gen rand) (2*)

randOdd :: Gen Integer
randOdd = liftM (Gen rand) (\i -> 2 * i + 1)

randTen :: Gen Integer
randTen = liftM (Gen rand) (10*)

randPair :: Gen (Char, Integer)
randPair = liftM2 randLetter (Gen rand) (\l i -> (l, i))

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair a b  = liftM2 a b (\x y -> (x, y))

repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = bind

-- Set 2

headMay :: [a] -> Maybe a
headMay [] = None
headMay (x:xs) = Some x

tailMay :: [a] -> Maybe [a]
tailMay [] = None
tailMay (x:xs) = Some xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay x ys = match >>= (\p -> Some (snd p))
  where match = headMay $ filter (\p -> (fst p) == x) ys

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = None
divMay x y = Some $ x / y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = None
maximumMay (x:xs) = Some $ foldl larger x xs
  where larger y z | y >= z = y
                   | y < z = z

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = None
minimumMay (x:xs) = Some $ foldl smaller x xs
  where smaller y z | y <= z = y
                    | y > z = z

queryGreek :: GreekData -> String -> Maybe Double
queryGreek db key = lookupMay key db >>=
                      (\ls -> join $ liftM2 (liftM (tailMay ls >>= maximumMay) fromIntegral)
                                            (liftM (headMay ls) fromIntegral)
                                            divMay)

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

tailProd :: Num a => [a] -> Maybe a
tailProd ls = liftM (tailMay ls) product

tailSum :: Num a => [a] -> Maybe a
tailSum ls = liftM (tailMay ls) sum

-- Set 3


