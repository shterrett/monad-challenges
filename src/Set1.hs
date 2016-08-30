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
randEven = generalB rand (2*)

randOdd :: Gen Integer
randOdd = generalB rand (\i -> 2*i - 1)

randTen :: Gen Integer
randTen = generalB rand (10*)

generalA :: (Integer -> Integer) -> Gen Integer
generalA transform = let output = \(i, seed) -> (transform i, seed)
                     in output . rand

generalB :: Gen a -> (a -> a) -> Gen a
generalB r t = let output = \(i, seed) -> (t i, seed)
               in output . r
