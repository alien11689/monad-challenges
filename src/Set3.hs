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
