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
