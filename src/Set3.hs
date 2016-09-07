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
allPairs (x:xs) ys = (map (\y -> (x, y))  ys) ++ (allPairs xs ys)

allCardsFromPairs :: [Int] -> [String] -> [Card]
allCardsFromPairs ranks suits = (map (uncurry Card)) (allPairs ranks suits)

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (x:xs) ys = (map (Card x) ys) ++ (allCards xs ys)

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs _ _ [] = []
allCombs f (x:xs) ys = (map (f x) ys) ++ (allCombs f xs ys)

allPairs2 :: [a] -> [b] -> [(a, b)]
allPairs2 = allCombs (,)

allCards2 :: [Int] -> [String] -> [Card]
allCards2 = allCombs Card
