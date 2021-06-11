-- | Miscellaneous examples of groups.
module Algebra.Finite.Group.Misc where

import Algebra.Finite.Group

import qualified Data.Set as Set

data ABC = A | B | C
  deriving (Eq, Show, Ord)

instance GroupElem ABC

-- | The group with three elements, isomorphic to @Z/3Z@.
abc :: Group ABC
abc = Group
  { gSet = Set.fromList [A,B,C]
  , gMul = (#)
  , gInv = inv
  , gId = A
  }
  where A # x = x
        x # A = x
        B # B = C
        B # C = A
        C # B = A
        C # C = B

        inv A = A
        inv B = C
        inv C = B
