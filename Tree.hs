module Tree where

import Data.List (foldl')

data Tree a = 
  Nil | Node a [Tree a]
  deriving(Eq, Ord, Show)

type Extent = [(Float, Float)]

movetree :: Tree (Char, Float) -> Float -> Tree (Char, Float)
movetree tree amount = case tree of
  Node(label, x) subtrees -> Node(label, x+amount) subtrees
  Nil -> Nil

moveextent :: Extent -> Float -> Extent
moveextent e x = map (\(a,b) -> (a+x, b+x)) e

merge :: Extent -> Extent -> Extent
merge left right = case (left,right) of
  ([], r) -> r
  (l, []) -> l
  ((l,_):ls, (_,r):rs) -> (l,r) : merge ls rs

mergelist :: Foldable t => t Extent -> Extent
-- what is the point of point-free programming? Why is currying useful here?
mergelist = foldl' merge [] 

rmax (l, r) = if l > r then l else r

fit :: Extent -> Extent -> Float
fit left right = case (left, right) of
  ((_, r):rs, (l,_):ls) -> rmax (fit ls rs, l - r + 1.0)
  (_,_) -> 0.0

