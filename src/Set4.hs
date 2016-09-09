{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

type Gen a = Seed -> (a, Seed)

data Maybe a = Some a | None
             deriving Eq

instance Show a => Show (Maybe a) where
    show (Some a) = show a
    show None = "None"

generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
yLink ::  (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c

generalA2 :: Gen a -> (a -> a) -> Gen a
transMaybe :: (a -> b) -> Maybe a -> Maybe b


genTwo :: Gen a -> (a -> Gen b) -> Gen b
link :: Maybe a -> (a -> Maybe b) -> Maybe b

mkGen :: a -> Gen a
mkMaybe :: a -> Maybe a
