{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Some a | None

instance Show a => Show (Maybe a) where
    show (Some a) = show a
    show None = "None"
