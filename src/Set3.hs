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

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs _ _ [] = []
allCombs f (x:xs) ys = (combs x ys) ++ (allCombs f xs ys)
  where combs e [] = []
        combs e (z:zs) = (f e z):(combs e zs)

allPairs2 :: [a] -> [b] -> [(a, b)]
allPairs2 = allCombs (,)

allCards2 :: [Int] -> [String] -> [Card]
allCards2 = allCombs Card
