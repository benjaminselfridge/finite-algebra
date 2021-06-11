-- | Groups of integers mod @n@.
module Algebra.Finite.Group.Zn
  ( znAdditive
  ) where

import Algebra.Finite.Group

import qualified Data.Set as Set

-- | The additive group of integers mod @n@.
znAdditive :: Integer -> Group Integer
znAdditive n = Group
  { gSet = Set.fromList [0..(n-1)]
  , gMul = \a b -> (a + b) `mod` n
  , gInv = \a -> negate a `mod` n
  , gId  = 0
  }
