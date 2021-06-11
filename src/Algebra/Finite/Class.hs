{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Type classes for finite algebraic structures and mappings between them.
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Algebra.Finite.Class
  ( Algebra(..)
  , checkAlgebra
  , Morphism(..)
  , checkMorphism
  , morphismTable
  ) where

import Algebra.Finite.Property

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (listToMaybe, mapMaybe)
import Control.Arrow (second)
import Data.Kind

import Prelude hiding (Functor(..))

checkLaws :: Set a -> [(law, Property a)] -> Maybe (law, [a])
checkLaws d laws =
  let results = [ (law, checkProperty p (Set.toList d)) | (law, p) <- laws]
      f (law, res) = case res of
        PropHolds -> Nothing
        PropFailed as -> Just (law, as)
  in listToMaybe (mapMaybe f results) -- Return the first law that fails.

-- | An algebra is just a set with some laws, which are usually stated in terms
-- of operations on the underlying set. The @p@ parameter allows the instance to
-- specify constraints on the element type of the underlying set.
class Algebra alg where
  -- | The type of laws for the algebra. These are typically just a simple
  -- enumeration type, one for each law.
  type AlgebraLaw alg :: Type

  -- | The valid types for the underlying set of the algebra. Usually @Eq@ is
  -- the bare minimum requirement.
  type AlgebraElem alg :: Type -> Constraint

  -- | The underlying set of the algebra.
  algebraSet :: AlgebraElem alg a => alg a -> Set a

  -- | The laws that must hold for the algebra.
  algebraLaws :: AlgebraElem alg a => [(AlgebraLaw alg, alg a -> Property a)]

  -- | The codomain of a morph

-- | Check that all the laws hold for a given algebra. If a law does not hold,
-- return @Just (law, inputs)@, where @law@ is the law that fails, and @inputs@
-- are the inputs to the law that constitute a counterexample.
checkAlgebra :: (Algebra alg, AlgebraElem alg a) => alg a -> Maybe (AlgebraLaw alg, [a])
checkAlgebra alg = checkLaws (algebraSet alg) (second ($ alg) <$> algebraLaws)

-- | A morphism of algebras is a mapping from one algebra to another. The laws
-- for a valid morphism will vary depending on what the algebra is. Typically,
-- the morphism must respect all the operations of the algebra, and it must also
-- be a valid function (i.e. it must map elements of the domain to elements of
-- the codomain).
class Algebra alg => Morphism m alg | m -> alg, alg -> m where
  type MorphismLaw m :: Type

  -- | The underlying map of the morphism.
  morphism         :: (AlgebraElem alg a, AlgebraElem alg b) => m a b -> a -> b

  -- | The domain of the morphism.
  morphismDomain   :: (AlgebraElem alg a, AlgebraElem alg b) => m a b -> alg a

  -- | The codomain of the morphism.
  morphismCodomain :: (AlgebraElem alg a, AlgebraElem alg b) => m a b -> alg b

  -- | The morphism laws.
  morphismLaws     :: (AlgebraElem alg a, AlgebraElem alg b) => [(MorphismLaw m, m a b -> Property a)]

-- | Check that all the laws hold for a given morphism. If a law does not hold,
-- return @Just (law, inputs)@, where @law@ is the law that fails, and @inputs@
-- are the inputs to the law that constitute a counterexample.
checkMorphism :: (AlgebraElem alg a, AlgebraElem alg b, Morphism m alg) => m a b -> Maybe (MorphismLaw m, [a])
checkMorphism m = checkLaws (algebraSet (morphismDomain m)) (second ($ m) <$> morphismLaws)

-- | Build a concrete table corresponding to a morphism.
morphismTable :: (AlgebraElem alg a, AlgebraElem alg b, Morphism m alg) => m a b -> [(a, b)]
morphismTable m = [ (a, morphism m a)
                  | a <- Set.toList (algebraSet (morphismDomain m))
                  ]
