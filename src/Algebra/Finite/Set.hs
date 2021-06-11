-- | The algebra of finite sets.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Algebra.Finite.Set
  ( Set(..)
  , SetElem
  , SetLaw
  -- * Homomorphisms
  , SetHomomorphism(..)
  , SetHomomorphismLaw(..)
  ) where

import Algebra.Finite.Class
import Algebra.Finite.Property

import qualified Data.Set as Set

-- | A 'Set' is simply a collection of elements with no associated operations.
newtype Set a = Set { unSet :: Set.Set a }

instance Show (Set a) where
  show _ = "<set>"

-- | The only requirement for the elements of a set is that we can insert them
-- into a set container.
class Ord a => SetElem a

-- | There are no laws that must be satisfied; any collection of elements is a
-- valid set.
data SetLaw

instance Algebra Set where
  type AlgebraLaw Set = SetLaw
  type AlgebraElem Set = SetElem

  algebraSet = unSet
  algebraLaws = []

-- | A set homomorphism is just a function from one set to another.
data SetHomomorphism a b = SetHomomorphism
  { shDomain :: Set a
  , shCodomain :: Set b
  , shMap :: a -> b
  }

-- | The only law that must be satisfied for a valid homomorphism is that it is
-- closed, i.e. that its range is a subset of its codomain.
data SetHomomorphismLaw = SHClosed
  deriving Show

instance Morphism SetHomomorphism Set where
  type MorphismLaw SetHomomorphism = SetHomomorphismLaw

  morphismDomain = shDomain
  morphismCodomain = shCodomain
  morphism = shMap
  morphismLaws = [ ( SHClosed
                   , \f -> Property $ \a -> shMap f a `elem` unSet (shCodomain f)
                   )
                 ]
