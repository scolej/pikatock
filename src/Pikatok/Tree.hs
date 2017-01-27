module Pikatok.Tree
  ( TagTree
  , TagVal(..)
  , blankTree
  , grabTreeM
  , mappendTree
  , simpleShowTree
  , sortTree
  , sortOrdTree
  , sumBelow
  , tagTreeInsert
  ) where

import Data.Foldable
import Data.List
import Data.Monoid
import Data.Tree

-- | Split a list with a predicate into the elements before the first
-- match, the first match, and the elements after the first match.
perforate :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
perforate f xs =
  if any f xs
    then let (as, b:bs) = break f xs
         in Just (as, b, bs)
    else Nothing

-- | Some value tagged with a string.
data TagVal a = TagVal
  { ttag :: String
  , tval :: a
  } deriving (Show)

type TagTree a = Tree (TagVal a)

blankTree
  :: Monoid m
  => TagTree m
blankTree = Node (TagVal "" mempty) []

tagTreeInsert
  :: Monoid a
  => TagTree a -> ([String], a) -> TagTree a
tagTreeInsert (Node tv children) (t:ts, a) =
  let perf = perforate (\(Node (TagVal s _) _) -> s == t) children
      ins tree = tagTreeInsert tree (ts, a) -- There are some more sub-tags, we are going to need to keep inserting.
  in case perf of
       Just (as, x, bs) -> Node tv $ as ++ ins x : bs
       Nothing -> Node tv $ ins (Node (TagVal t mempty) []) : children
-- Run out of tags to insert, just mappend onto the value we are at.
tagTreeInsert (Node (TagVal t v) children) ([], a) =
  Node (TagVal t (v <> a)) children

-- | Build a tag tree from a list of things which can be tagged and
-- mappended.
mappendTree
  :: Monoid m
  => (a -> ([String], m))
  -- ^ A function which maps a thing to a list of tags and a Monoid
  -- value.
  -> [a]
  -- ^ List of things to collect into the TagTree.
  -> TagTree m
mappendTree f es =
  let es' = map f es
  in foldl tagTreeInsert blankTree es'

sumBelow
  :: Monoid m
  => TagTree m -> TagTree m
sumBelow tree@(Node tagval cs) =
  let v' = fold (fmap tval tree)
  in Node tagval {tval = v'} (map sumBelow cs)

sortTree :: (a -> a -> Ordering) -> Tree a -> Tree a
sortTree f (Node v cs) = Node v cs'
  where
    cs' = map (sortTree f) (sortBy (\a b -> f (g a) (g b)) cs)
    g (Node t _) = t -- Value extractor.

-- | Make sure all of a trees children are sorted in descending order.
sortOrdTree
  :: Ord o
  => Tree o -> Tree o
sortOrdTree = sortTree compare

simpleShowTree :: (a -> String) -> Tree a -> String
simpleShowTree f = go 0
  where
    go i (Node x cs) =
      replicate i ' ' ++ " " ++ f x ++ "\n" ++ concatMap (go (i + 4)) cs

-- | Grab out a subtree from a tree using a sequence of tags (if it exists).
grabTreeM
  :: Monoid a
  => TagTree a -> [String] -> Maybe (TagTree a)
grabTreeM tree [] = Just tree
grabTreeM (Node _ cs) ts =
  let tag = head ts
      mt = find (\(Node (TagVal t _) _) -> t == tag) cs -- Child tree matching the head tag if it exists.
  in case mt of
       Nothing -> Nothing
       Just t -> grabTreeM t (tail ts)
