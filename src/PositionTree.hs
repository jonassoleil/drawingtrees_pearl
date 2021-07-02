module PositionTree where

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
mergelist = foldl' merge [] 

rmax (l, r) = if l > r then l else r

fit :: Extent -> Extent -> Float
fit left right = case (left, right) of
  ((_, r):rs, (l,_):ls) -> rmax (fit ls rs, l - r + 1.0)
  (_,_) -> 0.0

fitlistl es =
  let 
    fitlistl' acc lst = case lst of 
      [] -> []
      e:es -> 
          let x = fit acc e in 
            (x : fitlistl' (merge acc (moveextent e x)) es)
  in fitlistl' [] es

flipextent:: Extent -> Extent 
flipextent = map (\(l,r) -> (-r, -l)) 
mean x y = (x+y)/2.0

fitlistr = reverse . map (\x -> - x). fitlistl . map flipextent . reverse

fitlist es = zipWith mean (fitlistl es) (fitlistr es)

design :: Tree Char -> Tree (Char, Float)
design tree = fst (design' tree)
  where 
    design' tree = case tree of
      Nil -> (Nil, [(0.0, 0.0)])
      Node label subtrees -> 
        let (trees, extents) = unzip (map design' subtrees) in
        let positions = fitlist extents in
        let ptrees = zipWith movetree trees positions in
        let pextents = zipWith moveextent extents positions in
        let resultextent = (0.0, 0.0) : mergelist pextents in
        let resulttree = Node (label, 0.0) ptrees
        in (resulttree, resultextent)