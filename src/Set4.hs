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
sequence (x:xs) = yLink (toList x) (sequence xs) (\b c -> b ++ c)
  where toList m1 = bind m1 (\y -> return [y])

liftM2 :: Monad m => m a -> m b -> (a -> b -> c) -> m c
liftM2 = yLink

liftM3 :: Monad m => m a -> m b -> m c ->  (a -> b -> c -> d) -> m d
liftM3 m1 m2 m3 f = bind m3 (\v3 -> bind mg (\g -> return (g v3)))
    where mg = liftM2 m1 m2 f -- Monad (c -> d)

join :: Monad m => m (m a) -> m a
join x = bind x id

chain :: Monad m => (a -> m b) -> m a -> m b
chain = flip bind

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = liftM2 mf ma (\f va -> f va)
