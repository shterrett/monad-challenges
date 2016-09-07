{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a, b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) ys = (pairs x ys) ++ (allPairs xs ys)
  where pairs e [] = []
        pairs e (z:zs) = (e, z):(pairs e zs)
