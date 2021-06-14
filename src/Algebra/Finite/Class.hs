{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Type classes for finite algebraic structures and mappings between them.
module Algebra.Finite.Class
  ( Algebra(..)
  , checkAlgebra
  , Morphism(..)
  , checkMorphism
  , morphismTable
  -- * Type classes for algebras
  , SemigroupLike(..)
  , semigroupLaws
  , ppMulTable
  , MonoidLike(..)
  , monoidLaws
  , GroupLike(..)
  , groupLaws
  ) where

import Algebra.Finite.Property

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (listToMaybe, mapMaybe)
import Control.Arrow (second)
import Data.Kind

import Prelude hiding (Functor(..))
import Data.Proxy (Proxy(Proxy))
import qualified Text.PrettyPrint.Boxes as B

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

  -- | The morphism laws. This should include a closure law representing the
  -- fact that @forall a in alg . phi(a) in alg'@.
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

-- | Class for semigroups, or algebras with an associative binary operation.
class Algebra alg => SemigroupLike alg where
  -- | Semigroup binary operation.
  sgMul       :: alg a -> a -> a -> a
  -- | Associativity of the operation.
  sgMulAssoc  :: proxy alg -> AlgebraLaw alg
  -- | Closure of the operation.
  sgMulClosed :: proxy alg -> AlgebraLaw alg

-- | Semigroup laws, just associativity of multiplication.
semigroupLaws :: forall alg a . (AlgebraElem alg a, Eq a, SemigroupLike alg)
              => [( AlgebraLaw alg, alg a -> Property a)]
semigroupLaws = [ ( sgMulClosed (Proxy @alg)
                  , \alg -> Property $ \a b -> sgMul alg a b `elem` algebraSet alg)
                , ( sgMulAssoc (Proxy @alg)
                  , \alg -> Property $ \a b c -> sgMul alg (sgMul alg a b) c == sgMul alg a (sgMul alg b c))
                ]

ppMulTable' :: (SemigroupLike alg, Show a, AlgebraElem alg a) => alg a -> B.Box
ppMulTable' g = B.punctuateH B.center1 vbar allCols
  where as = Set.toList (algebraSet g)

        leftmostCol = B.punctuateV B.center1 hbar $
                      B.char ' ' : [ B.text (show a) | a <- as ]

        mkCol b = B.punctuateV B.center1 hbar $
                  B.text (show b) : [ B.text (show (sgMul g a b)) | a <- as ]

        allCols = leftmostCol : [ mkCol b | b <- as ]

        vbar = B.vsep 0 B.center1 ([ B.char '|' B.// B.char '+' | _ <- as ] ++ [B.char '|'])
        hbar = B.hcat B.center1 $ replicate (maximum [length (show a) | a <- as] ) (B.char '-')

-- | Pretty-print a semigroup's multiplication table.
ppMulTable :: (SemigroupLike alg, Show a, AlgebraElem alg a) => alg a -> String
ppMulTable g = B.render (ppMulTable' g)

class SemigroupLike alg => MonoidLike alg where
  -- | Monoidal identity element.
  mId            :: alg a -> a
  -- | Identity closure law.
  mIdClosed      :: proxy alg -> AlgebraLaw alg
  -- | Left identity law.
  mLeftIdentity  :: proxy alg -> AlgebraLaw alg
  -- | Right identity law.
  mRightIdentity :: proxy alg -> AlgebraLaw alg

-- | Monoid laws. This includes all the semigroup laws, as well as 'mIdClosed',
-- 'mLeftIdentity', and 'mRightIdentity'.
monoidLaws :: forall alg a . (AlgebraElem alg a, Eq a, MonoidLike alg)
           => [( AlgebraLaw alg, alg a -> Property a)]
monoidLaws = semigroupLaws ++
             [ ( mIdClosed (Proxy @alg)
               , \alg -> Property $ mId alg `elem` algebraSet alg)
             , ( mLeftIdentity (Proxy @alg)
               , \alg -> Property $ \a -> sgMul alg a (mId alg) == a)
             , ( mRightIdentity (Proxy @alg)
               , \alg -> Property $ \a -> sgMul alg (mId alg) a == a)
             ]

class MonoidLike alg => GroupLike alg where
  -- | Group inverse unary operation.
  gInv             :: alg a -> a -> a
  -- | Inverse closure law.
  gInvClosed       :: proxy alg -> AlgebraLaw alg
  -- | Left inverse law.
  gInvLeftInverse  :: proxy alg -> AlgebraLaw alg
  -- | Right inverse law.
  gInvRightInverse :: proxy alg -> AlgebraLaw alg

-- | Group laws. This includes all the semigroup and monoid laws, as well as
-- 'gInvClosed', 'gInvLeftInverse', and 'gInvRightInverse'.
groupLaws :: forall alg a . (AlgebraElem alg a, Eq a, GroupLike alg)
          => [(AlgebraLaw alg, alg a -> Property a)]
groupLaws = monoidLaws ++
            [ ( gInvClosed (Proxy @alg)
              , \g -> Property $ \a -> gInv g a `elem` algebraSet g )
            , ( gInvLeftInverse (Proxy @alg)
              , \g -> Property $ \a -> sgMul g (gInv g a) a == mId g )
            , ( gInvRightInverse (Proxy @alg)
              , \g -> Property $ \a -> sgMul g a (gInv g a) == mId g )
            ]
