-- | Permutation groups.
module Algebra.Finite.Group.Symmetric
  ( Permutation(..)
  , fromList
  , sn
  ) where

import Algebra.Finite.Group ( Group(..) )
import qualified Algebra.Finite.Set as Set

import qualified Math.Combinat.Permutations as MCP

-- | Permutation of n elements.
newtype Permutation = Permutation { getPermutation :: MCP.Permutation }
  deriving (Eq, Ord)

-- | Construct a permutation from a list of integers. It is assumed that the
-- input list is a valid permutation; this is not checked.
fromList :: [Integer] -> Permutation
fromList is = Permutation (MCP.toPermutationUnsafe (fromInteger <$> is))

instance Show Permutation where
  show (Permutation p) = show (MCP.fromPermutation p)

-- | Symmetric (permutation) group on @n@ elements.
sn :: Integer -> Group Permutation
sn n = Group
  { set = Set.fromList (Permutation <$> MCP.permutations (fromInteger n))
  , mul = \(Permutation p) (Permutation q) ->
             Permutation (MCP.multiplyPermutation  p q)
  , inv = \(Permutation p) -> Permutation (MCP.inversePermutation p)
  , e = Permutation (MCP.identityPermutation (fromInteger n))
  }
