{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs _ [] = []
allPairs xx yy = genPairs [] xx yy
    where genPairs acc [] _ = acc
          genPairs acc (_:xs) [] = genPairs acc xs yy
          genPairs acc xxx@(x:_) (y:ys) = genPairs (acc ++ [(x,y)]) xxx ys

data Card = Card Int String

instance Show Card where
    show (Card rank suite) = show rank ++ suite

allCards :: [Int] -> [String] -> [Card]
allCards xx yy = myMap (uncurry Card) (allPairs xx yy)
    where myMap _ [] = []
          myMap f (x:xs) = f x : myMap f xs
