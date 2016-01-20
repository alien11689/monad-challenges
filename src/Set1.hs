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
randEven = generalA (*2) rand

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA (+1) randEven

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (*10) rand

generalA :: (a -> b) -> Gen a -> Gen b
generalA f gen seed = let (new, newSeed) = gen seed
                      in (f new, newSeed)

randPair :: Gen (Char, Integer)
randPair seed = let (first, newSeed) = randLetter seed
                    (second, secondSeed) = rand newSeed
                in ((first, second), secondSeed)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb seed = let (first, newSeed) = ga seed
                             (second, secondSeed) = gb newSeed
                         in ((first, second), secondSeed)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb seed = let (first, newSeed) = ga seed
                            (second, secondSeed) = gb newSeed
                        in (f first second, secondSeed)

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (\x y -> (x,y))

repRandom :: [Gen a] -> Gen [a]
repRandom (x:[]) seed = generalA (\a -> [a]) x seed
repRandom (x:xs) seed = generalB (\a b -> a:b) x (repRandom xs) seed
