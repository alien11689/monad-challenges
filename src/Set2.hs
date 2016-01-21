{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "Just " ++ show x

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x ((a,b):ab)
    | x == a = Just b
    | otherwise = lookupMay x ab

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just $ x / y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just $ findMax x xs
    where findMax cur [] = cur
          findMax cur (y:ys)
            | cur < y = findMax y ys
            | otherwise = findMax cur ys

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just $ findMin x xs
    where findMin cur [] = cur
          findMin cur (y:ys)
            | cur > y = findMin y ys
            | otherwise = findMin cur ys

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd key = let xs = lookupMay key gd
                        max = superMaximum $ superTail xs
                        h = superHead xs
                    in superDiv max h
                    where
                        superDiv Nothing _ = Nothing
                        superDiv _ Nothing = Nothing
                        superDiv (Just a) (Just b) = divMay (fromIntegral a) (fromIntegral b)
                        superTail Nothing = Nothing
                        superTail (Just a) = tailMay a
                        superMaximum Nothing = Nothing
                        superMaximum (Just a) = maximumMay a
                        superHead Nothing = Nothing
                        superHead (Just a) = headMay a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing _ = Nothing
link (Just x) f = f x

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain = flip link

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd key = let xs = lookupMay key gd
                         t = link xs tailMay
                         max = link t maximumMay
                         h = link xs headMay
                    in superDiv max h
                    where superDiv (Just a) (Just b) = divMay (fromIntegral a) (fromIntegral b)
                          superDiv _ _ = Nothing

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f (Just x) (Just y) = Just (f x y)
yLink _ _ _ = Nothing

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries db p1 p2 = yLink (+) (lookupMay p1 db) (lookupMay p2 db)

mkMaybe :: a -> Maybe a
mkMaybe = Just

yLink2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink2 f (Just x) (Just y) = mkMaybe (f x y)
yLink2 _ _ _ = Nothing
