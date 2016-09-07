{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

data Card = Card Int String
instance Show Card where
    show (Card r s) = (show r) ++ s

allPairs :: [a] -> [b] -> [(a, b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) ys = (pairs x ys) ++ (allPairs xs ys)
  where pairs e [] = []
        pairs e (z:zs) = (e, z):(pairs e zs)

allCardsFromPairs :: [Int] -> [String] -> [Card]
allCardsFromPairs ranks suits = (map (uncurry Card)) (allPairs ranks suits)

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (x:xs) ys = (cards x ys) ++ (allCards xs ys)
  where cards e [] = []
        cards e (z:zs) = (Card  e z):(cards e zs)
