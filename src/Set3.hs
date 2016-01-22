{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = allPerms (,)

data Card = Card Int String

instance Show Card where
    show (Card rank suite) = show rank ++ suite

allCards :: [Int] -> [String] -> [Card]
allCards = allPerms Card

allPerms :: (a -> b -> c) -> [a] -> [b] -> [c]
allPerms f xx yy = genPerms [] xx yy
    where genPerms acc [] _ = acc
          genPerms acc (_:xs) [] = genPerms acc xs yy
          genPerms acc xxx@(x:_) (y:ys) = genPerms (acc ++ [f x y]) xxx ys

allPerms3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allPerms3 f xx yy zz = allPerms (\x (y,z) -> f x y z) xx (allPerms (,) yy zz)

permStep :: [a -> b] -> [a] -> [b]
permStep (f:fs) xx = permStep' f xx ++ permStep fs xx
    where permStep' f (x:xs) = f x : permStep' f xs
          permStep' _ [] = []
permStep [] _ = []

genFun :: (a -> b -> c) -> [a] -> [b -> c]
genFun f (x:xs) = f x : genFun f xs
genFun _ [] = []

allPerms' :: (a -> b -> c) -> [a] -> [b] -> [c]
allPerms' f a = permStep (genFun f a)

allPerms3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allPerms3' f a b = permStep (permStep (genFun f a) b)
