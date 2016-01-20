{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = choose 5 [] $ mkSeed 1
    where choose 0 acc _ = acc
          choose amount acc seed = let (new, newSeed) = rand seed 
                                   in choose (amount - 1) (acc ++ [new]) newSeed

randLetter :: Gen Char
randLetter seed = let (new, newSeed) = rand seed
                  in (toLetter new, newSeed)

randString3 :: String
randString3 = choose 3 [] $ mkSeed 1
    where choose 0 acc _ = acc
          choose amount acc seed = let (new, newSeed) = randLetter seed
                                   in choose (amount - 1) (acc ++ [new]) newSeed

randEven :: Gen Integer -- the output of rand * 2
randEven seed = generalA (*2) rand seed

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA (+1) randEven

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (*10) rand

generalA :: (a -> b) -> Gen a -> Gen b
generalA f gen seed = let (new, newSeed) = gen seed
                      in (f new, newSeed)
