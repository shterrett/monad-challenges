{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

buildList :: Gen a -> Integer -> Seed -> [a] -> [a]
buildList _ 0 _ l = l
buildList randFn i s l =
    let (r, ns) = randFn s
    in  [r] ++ (buildList randFn (i - 1) ns l)

fiveRands :: [Integer]
fiveRands = buildList rand 5 (mkSeed 1) []

randomLetter :: Gen Char
randomLetter s =
    let (i, newSeed) = rand s
    in (toLetter i, newSeed)

randString3 :: String
randString3 = buildList randomLetter 3 (mkSeed 1) []

randEven :: Gen Integer
randEven = generalA2 rand (2*)

randOdd :: Gen Integer
randOdd = generalA2 rand (\i -> 2*i - 1)

randTen :: Gen Integer
randTen = generalA2 rand (10*)

generalA :: (Integer -> Integer) -> Gen Integer
generalA transform = let output = \(i, seed) -> (transform i, seed)
                     in output . rand

generalA2 :: Gen a -> (a -> a) -> Gen a
generalA2 r t = let output = \(i, seed) -> (t i, seed)
               in output . r

randPair :: Gen (Char, Integer)
randPair s = let (letter, seed) = randomLetter s
                 (number, seed2) = rand seed
             in ((letter, number), seed2)

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair f g s = let (val1, seed1) = f s
                        (val2, seed2) = g seed1
                    in ((val1, val2), seed2)

generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalB f g t s = let (val1, seed1) = f s
                       (val2, seed2) = g seed1
                   in (t val1 val2, seed2)

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 f g = generalB f g (\a b -> (a, b))

repRandom :: [Gen a] -> Seed -> ([a], Seed)
repRandom [] seed = ([], seed)
repRandom (g:gs) seed = let (val, s) = g seed
                            (vals, s2) = repRandom gs s
                        in ([val] ++ vals, s2)
