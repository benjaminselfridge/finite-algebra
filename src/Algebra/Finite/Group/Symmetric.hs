-- | Permutation groups.
module Algebra.Finite.Group.Symmetric
  ( Permutation(..)
  , fromList
  , cycleLeft
  , reflection
  -- * Groups of permutations
  , sn
  , dn
  ) where

import Algebra.Finite.Group ( Group(..), generated )
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

-- | The permutation that adds @1@ to each element of @[1..n], modulo @n@.
cycleLeft :: Integer -> Permutation
cycleLeft = Permutation . MCP.cycleLeft . fromInteger

-- | The permutation that reflects a regular @n@-gon about the point @1@ (so @1@
-- is fixed, and the remaining points are flipped along the axis crossing
-- through @1@).
reflection :: Integer -> Permutation
reflection n = fromList [ ((n - i + 1) `mod` n) + 1 | i <- [1..n] ]

-- | Dihedral group on @n@ elements. @dn n@ is a subgroup of @sn n@.
dn :: Integer -> Group Permutation
dn n = generated (sn n) (Set.fromList gens)
  where gens = [ cycleLeft n, reflection n ]
