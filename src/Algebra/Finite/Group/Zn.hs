-- | Groups of integers mod @n@.
module Algebra.Finite.Group.Zn
  ( znAdditive
  ) where

import Algebra.Finite.Group
import Algebra.Finite.Set

import Prelude hiding (id)

-- | The additive group of integers mod @n@.
znAdditive :: Integer -> Group Integer
znAdditive n = Group
  { set = fromList [0..(n-1)]
  , mul = \a b -> (a + b) `mod` n
  , inv = \a -> negate a `mod` n
  , e   = 0
  }
