{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

buildList :: (Seed -> (a, Seed)) -> Integer -> Seed -> [a] -> [a]
buildList _ 0 _ l = l
buildList randFn i s l =
    let (r, ns) = randFn s
    in  [r] ++ (buildList randFn (i - 1) ns l)

fiveRands :: [Integer]
fiveRands = buildList rand 5 (mkSeed 1) []

randomLetter :: Seed -> (Char, Seed)
randomLetter s =
    let (i, newSeed) = rand s
    in (toLetter i, newSeed)

randString3 :: String
randString3 = buildList randomLetter 3 (mkSeed 1) []
