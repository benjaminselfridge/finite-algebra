-- | Miscellaneous examples of groups.
module Algebra.Finite.Group.Misc where

import Algebra.Finite.Group

import qualified Data.Set as Set

data ABC = A | B | C
  deriving (Eq, Show, Ord)

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

-- 0: id
-- 1: (1 2)
-- 2: (1 3)
-- 3: (2 3)
-- 4: (1 2 3)
-- 5: (1 3 2)
s3 :: Group Integer
s3 = Group
  { gSet = Set.fromList [0..5]
  , gMul = (#)
  , gInv = inv
  , gId = 0
  }
  where 0 # a = a
        a # 0 = a
        1 # 1 = 0
        1 # 2 = 5
        1 # 3 = 4
        1 # 4 = 3
        1 # 5 = 2
        2 # 1 = 4
        2 # 2 = 0
        2 # 3 = 5
        2 # 4 = 1
        2 # 5 = 3
        3 # 1 = 5
        3 # 2 = 4
        3 # 3 = 0
        3 # 4 = 2
        3 # 5 = 1
        4 # 1 = 2
        4 # 2 = 3
        4 # 3 = 1
        4 # 4 = 5
        4 # 5 = 0
        5 # 1 = 3
        5 # 2 = 1
        5 # 3 = 2
        5 # 4 = 0
        5 # 5 = 4
        a # b = error $ "bad args to #: " ++ show a ++ " # " ++ show b

        inv 0 = 0
        inv 1 = 1
        inv 2 = 2
        inv 3 = 3
        inv 4 = 5
        inv 5 = 4
        inv a = error $ "bad arg to inv: inv " ++ show a
