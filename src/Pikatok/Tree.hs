module Pikatok.Tree
  ( TagTree
  , TagVal (..)
  , blankTree
  , mappendTree
  , simplePrintTree
  , sortTree
  , sumBelow
  , tagTreeInsert
  ) where

import Data.Foldable
import Data.List
import Data.Monoid
import Data.Ord
import Data.Tree

-- | Split a list with a predicate into the elements before the first
-- match, the first match, and the elements after the first match.
perforate :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
perforate f xs = if any f xs
                 then let (as, b:bs) = break f xs
                      in Just (as, b, bs)
                 else Nothing

-- | Some value tagged with a string.
data TagVal a = TagVal { ttag :: String
                       , tval :: a
                       }
  deriving Show

type TagTree a = Tree (TagVal a)

blankTree :: Monoid m => TagTree m
blankTree = Node (TagVal "" mempty) []

tagTreeInsert :: Monoid a =>  TagTree a -> ([String], a) -> TagTree a
tagTreeInsert (Node tv children) (t:ts, a) =
  let perf = perforate (\(Node (TagVal s _) _) -> s == t) children
      ins tree = tagTreeInsert tree (ts, a) -- There are some more sub-tags, we are going to need to keep inserting.
  in case perf of Just (as, x, bs) -> Node tv $ as ++ ins x : bs
                  Nothing -> Node tv $ ins (Node (TagVal t mempty) []) : children
-- Run out of tags to insert, just mappend onto the value we are at.
tagTreeInsert (Node (TagVal t v) children) ([], a) = Node (TagVal t (v <> a)) children

mappendTree :: Monoid m => (a -> ([String], m)) -> [a] -> TagTree m
mappendTree f es = let es' = map f es
                   in foldl tagTreeInsert blankTree es'

sumBelow :: Monoid m => TagTree m -> TagTree m
sumBelow tree@(Node tagval cs) =
  let v' = fold (fmap tval tree)
  in Node tagval {tval = v'} (map sumBelow cs)

sortTree :: Ord o => TagTree o -> TagTree o
sortTree (Node tv cs) = Node tv ((sortBy . comparing . fmap Down) f cs')
  where f (Node (TagVal _ v) _) = v
        cs' = map sortTree cs

simplePrintTree :: (a -> String) -> Tree a -> String
simplePrintTree f = go 0
  where go i (Node x cs) =
          replicate i ' ' ++ " " ++ f x ++ "\n" ++ concatMap (go (i + 4)) cs
