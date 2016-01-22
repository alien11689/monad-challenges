{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude

type Gen a = Seed -> (a, Seed)
-- generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- genTwo :: Gen a -> (a -> Gen b) -> Gen b

data Maybe a = Nothing | Just a
-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c

class M m where
    apply :: (a -> b -> c ) -> m a -> m b -> m c
    flatMap :: m a -> (a -> m b) -> m b
