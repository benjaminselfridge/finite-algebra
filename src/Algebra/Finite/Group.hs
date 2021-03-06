{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The algebra of finite groups.
module Algebra.Finite.Group
  ( -- * Groups
    Group(..)
  , muls
  , GroupElem
  , GroupLaw(..)
  , renaming
  , integerRenaming
  , groupMulTable
  , groupInvTable
  -- * Subgroups
  , generated
  , conjugateSet
  , checkNormalSubgroupOf
  , isNormalSubgroupOf
  -- * Cosets and quotient groups
  , leftCoset
  , rightCoset
  , multiplySets
  , inverseSet
  , leftQuotientGroup
  , rightQuotientGroup
  , quotientGroup
  -- * Direct products
  , directProduct
  -- * Homomorphisms
  , GroupHomomorphism(..)
  , GroupHomomorphismLaw(..)
  , kernel
  , image
  , generatedHomomorphism
  , restrictHomomorphism
  -- * Centralizers and normalizers
  , centralizer
  ) where

import Algebra.Finite.Class
import Algebra.Finite.Property
import Algebra.Finite.Set

import Data.Bifunctor (bimap)
import Data.Maybe (fromJust, isNothing)
import Data.Tuple (swap)
import Data.Foldable (find)

-- | A finite group has a set of elements, a multiplication operation, an
-- inverse operation, and an identity element. Laws are closure for the three
-- operations, associativity of multiplication, left\/right inverses, and
-- left\/right identity.
data Group a = Group
  { set :: Set a       -- ^ Underlying set
  , mul :: a -> a -> a -- ^ Group multiplication
  , inv :: a -> a      -- ^ Multiplicative inverse
  , e   :: a           -- ^ Multiplicative identity
  }

muls :: Group a -> [a] -> a
muls g = foldr (mul g) (e g)

-- | Construct the multiplication table from a group.
groupMulTable :: Ord a => Group a -> [(a, [(a, a)])]
groupMulTable g = [ (a, [ (b, mul g a b) | b <- d ]) | a <- d ]
  where d = toList (set g)

-- | Construct the inversion table from a group.
groupInvTable :: Ord a => Group a -> [(a, a)]
groupInvTable g = [ (a, gInv g a) | a <- toList (set g) ]

-- | Rename the elements of a group with a lookup table mapping elements to
-- elements. The table must be injective, and injectivity is not checked by
-- this function.
renaming :: (Ord a, Ord b) => [(a, b)] -> Group a -> GroupHomomorphism a b
renaming table g = GroupHomomorphism { ghDomain = g
                                     , ghCodomain = h
                                     , ghMap = to
                                     }
  where h = Group { set = fromList [ to a | a <- toList (set g) ]
                  , mul = \b b' -> to (mul g (from b) (from b'))
                  , inv = to . inv g . from
                  , e = to (e g)
                  }
        to   a = fromJust $ lookup a table
        from b = fromJust $ lookup b (swap <$> table)

-- | Rename the elements of a group to be integers, mapping the identity element
-- to @0@.
integerRenaming :: forall a . Ord a => Group a -> GroupHomomorphism a Integer
integerRenaming g = renaming table g
  where table = (e g, 0) : zip (toList (delete (e g) (set g))) [1..]

instance Show (Group a) where
  show _ = "<group>"

-- | The laws that every valid group must satisfy.
data GroupLaw = IdClosed        -- ^ @e in g@.
              | InvClosed       -- ^ @forall a in g . inv a in g@.
              | MulClosed       -- ^ @forall a b in g . a * b in g@.
              | IdLeftIdentity  -- ^ @forall a in g . e * a = a@.
              | IdRightIdentity -- ^ @forall a in g . a * e = a@.
              | InvLeftInverse  -- ^ @forall a in g . inv a * a = e@.
              | InvRightInverse -- ^ @forall a in g . a * inv a = e@.
              | MulAssoc        -- ^ @forall a b c in g . (a * b) * c = a * (b * c)@.
  deriving Show

-- | The only requirement for the elements of a group is that we can insert them
-- into a set container.
class    Ord a => GroupElem a
instance Ord a => GroupElem a

instance Algebra Group where
  type AlgebraLaw Group = GroupLaw
  type AlgebraElem Group = GroupElem

  algebraSet g = let Set s = set g in s
  algebraLaws = groupLaws

instance SemigroupLike Group where
  sgMul = mul
  sgMulAssoc _ = MulAssoc
  sgMulClosed _ = MulClosed

instance MonoidLike Group where
  mId = e
  mIdClosed _ = IdClosed
  mLeftIdentity _ = IdLeftIdentity
  mRightIdentity _ = IdRightIdentity

instance GroupLike Group where
  gInv = inv
  gInvClosed _ = InvClosed
  gInvLeftInverse _ = InvLeftInverse
  gInvRightInverse _ = InvRightInverse

-- | @MulTriple a b c@ means @a * b = c@ in some group. The @Eq@ and @Ord@
-- instances only compares the result @c@.
data MulTriple a = MulTriple a a a
  deriving Show

instance Eq a => Eq (MulTriple a) where
  MulTriple _ _ a == MulTriple _ _ a' = a == a'

instance Ord a => Ord (MulTriple a) where
  compare (MulTriple _ _ a) (MulTriple _ _ a') = compare a a'

-- | Get the full set of triples generated by a subset of a group's elements.
-- This can be used to construct homomorphisms.
generatedTriples :: Ord a => Group a -> Set a -> Set (MulTriple a)
generatedTriples g aSet = gen (fromList [ MulTriple (e g) a a | a <- toList (insert (e g) aSet)])
  where gen s | s == s' = s
              | otherwise = gen (s `union` s') -- left-biased to preserve generativity
          where s' = multiplySetsTriples g rs rs
                rs = fromList [ a | MulTriple _ _ a <- toList s ]

-- | Get the subgroup generated by some subset of a group.
generated :: Ord a => Group a -> Set a -> Group a
generated g aSet = g { set = gen (insert (e g) aSet) }
  where gen s | s' == s = s
              | otherwise = gen s'
          where s' = multiplySets g s s

-- | Given a set @S@ and a group element @a@, compute the set @{ a * x * a^-1 |
-- x <- S }@.
conjugateSet :: Ord a => Group a -> a -> Set a -> Set a
conjugateSet g a s = fromList [ mul g a (mul g x (inv g a)) | x <- toList s ]

-- | Check that the first group is a normal subgroup of the second, returning a
-- counterexample @a@ satisfying @a * h * a^-1 /= h@ if one exists.
checkNormalSubgroupOf :: Ord a => Group a -> Group a -> Maybe a
checkNormalSubgroupOf h g = case checkProperty p (toList (set g)) of
  PropHolds      -> Nothing
  PropFailed [a] -> Just a
  _ -> error "PANIC: bug in checkNormalSubgroupOf"
  where p = Property $ \a -> conjugateSet g a (set h) == set h

-- | Ensure that a subgroup is normal.
isNormalSubgroupOf :: Ord a => Group a -> Group a -> Bool
isNormalSubgroupOf h g = isNothing (checkNormalSubgroupOf h g)

-- | A group homomorphism is just a map on the underlying sets that respects
-- multiplication.

data GroupHomomorphism a b = GroupHomomorphism
  { ghDomain   :: Group a
  , ghCodomain :: Group b
  , ghMap      :: a -> b
  }

-- | The laws that every group homomorphism must satisfy.
data GroupHomomorphismLaw = GHRespectsMultiplication
                          | GHClosed
  deriving Show

instance Morphism GroupHomomorphism Group where
  type MorphismLaw GroupHomomorphism = GroupHomomorphismLaw

  morphismDomain = ghDomain
  morphismCodomain = ghCodomain
  morphism = ghMap
  morphismLaws = [ ( GHClosed
                   , \h -> Property $ \a -> ghMap h a `elem` set (ghCodomain h)
                   )
                 , ( GHRespectsMultiplication
                   , \h -> Property $ \a b -> ghMap h (mul (ghDomain h) a b) ==
                                              mul (ghCodomain h) (ghMap h a) (ghMap h b)
                   )
                 ]

-- | Build a homomorphism by specifying where a selection of elements map to.
--
-- >>> import Algebra.Finite.Group
-- >>> import Algebra.Finite.Group.Zn
-- >>> import Algebra.Finite.Class
-- >>> z3 = znAdditive 3
-- >>> z6 = znAdditive 6
-- >>> h = generatedHomomorphism z6 z3 [(1, 1)]
-- >>> morphismTable h
-- [(0,0),(1,1),(2,2),(3,0),(4,1),(5,2)]
-- >>> h' = generatedHomomorphism z6 z3 [(1, 2)]
-- >> morphismTable h'
-- [(0,0),(1,2),(2,1),(3,0),(4,2),(5,1)]
generatedHomomorphism :: (Ord a, Ord b)
                      => Group a
                      -> Group b
                      -> [(a, b)]
                      -> GroupHomomorphism a b
generatedHomomorphism g h table = GroupHomomorphism
  { ghDomain   = g { set = fromList [ a    | MulTriple _ _ a <- toList aTriples ] }
  , ghCodomain = h { set = fromList [ to a | MulTriple _ _ a <- toList aTriples ] }
  , ghMap      = to
  }
  where aTriples = generatedTriples g (fromList [ a | (a, _) <- table ])
        aTriple a = fromJust $ find (\(MulTriple _ _ a') -> a' == a) aTriples
        to a | a == e g = e h
             | Just b <- lookup a table = b
             | otherwise = let MulTriple x y _ = aTriple a
                           in mul h (to x) (to y)

-- | Restrict a homomorphism to a subgroup of its domain. Does not check that
-- the input group is in fact a subgroup of the domain.
restrictHomomorphism :: (Ord a, Ord b)
                     => GroupHomomorphism a b
                     -> Group a
                     -> GroupHomomorphism a b
restrictHomomorphism phi g = phi { ghDomain = g
                                 , ghCodomain = (ghCodomain phi) { set = fromList [ morphism phi a | a <- toList (set g) ] }
                                 }

-- | Get the kernel of a group homomorphism.
kernel :: (GroupElem a, GroupElem b) => GroupHomomorphism a b -> Group a
kernel phi = (ghDomain phi) { set = g' }
  where g' =  fromList [ a | a <- toList (set (ghDomain phi))
                           , morphism phi a == e (ghCodomain phi)
                           ]

-- | Get the image of a group homomorphism.
image :: (GroupElem a, GroupElem b) => GroupHomomorphism a b -> Group b
image phi = (ghCodomain phi) { set = g' }
  where g' = fromList [ morphism phi a | a <- toList (set (ghDomain phi)) ]

-- | Get a left coset of a subgroup.
leftCoset :: Ord a
          => a
          -> Group a
          -- ^ subgroup
          -> Set a
leftCoset a g = fromList [ mul g a b | b <- toList (set g) ]

-- | Get a right coset of a subgroup.
rightCoset :: Ord a
           => Group a
           -- ^ subgroup
           -> a
           -> Set a
rightCoset g a = fromList [ mul g b a | b <- toList (set g) ]

-- | Multiply two sets together to get another set, keeping track of the
-- elements that were multiplied together.
multiplySetsTriples :: Ord a
                    => Group a
                    -> Set a
                    -> Set a
                    -> Set (MulTriple a)
multiplySetsTriples g aSet bSet =
  fromList [ MulTriple a b (mul g a b) | a <- as, b <- bs ]
  where as = toList aSet
        bs = toList bSet

-- | Multiply two sets together to get another set. This is always a
-- well-defined operation on sets, but only gives rise to a quotient group if
-- the sets involved are cosets of a normal subgroup.
multiplySets :: Ord a
             => Group a
                -- ^ Group containing the sets
             -> Set a
             -- ^ set 1
             -> Set a
             -- ^ set 2
             -> Set a
multiplySets g aSet bSet =
  fromList [ r | MulTriple _ _ r <- toList (multiplySetsTriples g aSet bSet) ]

-- | Take the group inverse of every element in a set, forming a new set. When
-- the input set is a coset of a normal subgroup, this acts as an inverse
-- operation for the quotient group.
inverseSet :: Ord a
           => Group a
           -> Set a
           -> Set a
inverseSet g s = fromList [ inv g a | a <- toList s ]

-- | Given a parent group @g@ and a normal subgroup @h@, form the quotient group
-- @g/h@ of left cosets of @h@.
leftQuotientGroup :: Ord a
                  => Group a
                  -- ^ Parent group
                  -> Group a
                  -- ^ Subgroup (must be normal, or the cosets do not form a group)
                  -> Group (Set a)
leftQuotientGroup g h = Group
  { set = fromList [ leftCoset a h | a <- toList (set g) ]
  , mul = multiplySets g
  , inv = inverseSet g
  , e = set h
  }

-- | Given a parent group @g@ and a normal subgroup @h@, form the quotient group
-- @g/h@ of left cosets of @h@. Note that this should be the same as
-- 'leftQuotientGroup' when @h@ is normal, but if @h@ is not normal, the
-- quotient group isn't actually a group, and the cosets will differe in general
-- depending on whether we are taking left or right cosets.
rightQuotientGroup :: Ord a
                   => Group a
                   -- ^ Parent group
                   -> Group a
                   -- ^ Subgroup (must be normal, or the cosets do not form a group)
                   -> Group (Set a)
rightQuotientGroup g h = Group
  { set = fromList [ rightCoset h a | a <- toList (set g) ]
  , mul = multiplySets g
  , inv = inverseSet g
  , e = set h
  }

-- | Alias for 'leftQuotientGroup'.
quotientGroup :: Ord a => Group a -> Group a -> Group (Set a)
quotientGroup = leftQuotientGroup

-- | Direct product of two groups.
directProduct :: (Ord a, Ord b) => Group a -> Group b -> Group (a, b)
directProduct g h = Group
  { set = fromList [ (a, b) | a <- toList (set g), b <- toList (set h) ]
  , mul = \(a, b) (a', b') -> (mul g a a', mul h b b')
  , inv = bimap (inv g) (inv h)
  , e   = (e g, e h)
  }

-- | The centralizer of a group relative to a subset of its elements are the
-- elements of the group that fix elements of the subset under conjugation.
centralizer :: Ord a => Group a -> Set a -> Group a
centralizer g a = g { set = c }
  where c = fromList [ x | x <- toList (set g)
                         , all (\y -> muls g [x,y,inv g x] == y) (toList a)
                         ]
