module Algebra.Finite.Group.Symmetric where

import Algebra.Finite.Group

import Data.Maybe (fromJust)

newtype Permutation a = Permutation [(a, a)]
  deriving (Show)

permSet :: Permutation a -> [a]
permSet (Permutation p) = fst <$> p

applyPerm :: Eq a => Permutation a -> a -> a
applyPerm (Permutation p) a = fromJust (lookup a p)

-- | Compose two permutations as functions.
--
-- @p \`composePerms\` q@ corresponds to @applyPerm p . applyPerm q@.
composePerms :: Eq a => Permutation a -> Permutation a -> Permutation a
composePerms (Permutation p) (Permutation q) = Permutation
  [ (a, c) | (a, b) <- q, let c = fromJust (lookup b p) ]

selections :: [a] -> [(a, [a])]
selections [] = []
selections (x:xs) = (x,xs) : [ (y, x:ys) | (y, ys) <- selections xs ]

allPermutations' :: [a] -> [[a]]
allPermutations' [] = [[]]
allPermutations' xs =
  [ y : zs
  | (y, ys) <- selections xs
  , zs <- allPermutations' ys
  ]

allPermutations :: [a] -> [Permutation a]
allPermutations as = Permutation . zip as <$> allPermutations' as

