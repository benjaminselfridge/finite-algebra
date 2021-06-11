-- | Permutation groups.
module Algebra.Finite.Group.Symmetric
  ( Permutation(..)
  , fromList
  , sn
  ) where

import Algebra.Finite.Group ( Group(..) )

import qualified Data.Set as Set
import qualified Math.Combinat.Permutations as MCP

-- | Permutation of n elements.
newtype Permutation = Permutation { getPermutation :: MCP.Permutation }
  deriving (Eq, Ord)

fromList :: [Integer] -> Permutation
fromList is = Permutation (MCP.toPermutationUnsafe (fromInteger <$> is))

instance Show Permutation where
  show (Permutation p) = show (MCP.fromPermutation p)

sn :: Integer -> Group Permutation
sn n = Group
  { gSet = Set.fromList (Permutation <$> MCP.permutations (fromInteger n))
  , gMul = \(Permutation p) (Permutation q) ->
             Permutation (MCP.multiplyPermutation  p q)
  , gInv = \(Permutation p) -> Permutation (MCP.inversePermutation p)
  , gId = Permutation (MCP.identityPermutation (fromInteger n))
  }
