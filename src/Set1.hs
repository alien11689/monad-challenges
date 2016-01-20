{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

fiveRands :: [Integer]
fiveRands = choose 5 [] $ mkSeed 1
    where choose 0 acc _ = acc
          choose amount acc seed = let (new, newSeed) = rand seed 
                                   in choose (amount - 1) (acc ++ [new]) newSeed

randLetter :: Seed -> (Char, Seed)
randLetter seed = let (new, newSeed) = rand seed
                  in (toLetter new, newSeed)

randString3 :: String
randString3 = choose 3 [] $ mkSeed 1
    where choose 0 acc _ = acc
          choose amount acc seed = let (new, newSeed) = randLetter seed
                                   in choose (amount - 1) (acc ++ [new]) newSeed
