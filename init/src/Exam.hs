module Exam where
import Data.Monoid


rotate :: Int -> [a] -> [a]
rotate x li
    | x > 0 = drop x li ++ take x li
    | x == 0 = li
    | otherwise = rotate (length li + x) li 

lengthCPS :: [a] -> ((Int -> r) -> r)
lengthCPS [] = \k -> k 0
lengthCPS (_:li) = \k -> lengthCPS li (\prev -> k (1 + prev))

data Tree a = Leaf | Node (Tree a) a (Tree a)

data TraverseType = NLR | LNR | LRN

choose ty (x, y, z) = 
    case ty of 
        NLR -> (y, x, z)
        LNR -> (x, y, z)
        LRN -> (x, z, y)

traverseTree :: TraverseType -> (a -> m -> m) -> m -> Tree a -> m
traverseTree _ f a Leaf = a
traverseTree ttype f a (Node lTree el rTree) = 
    case ttype of
        NLR -> traverseTree NLR f (traverseTree NLR f (f el a) lTree) rTree
        LNR -> traverseTree LNR f (f el (traverseTree LNR f a lTree)) rTree
        LRN -> f el (traverseTree LRN f (traverseTree LRN f a lTree) rTree)


instance Foldable Tree where
    foldr = traverseTree LNR

nlr = traverseTree NLR (:) []

lnr = traverseTree LNR (:) []

lrn = traverseTree LRN (:) []