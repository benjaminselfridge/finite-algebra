{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Algebra.Finite.Class where

import Algebra.Finite.Property

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (listToMaybe, mapMaybe)
import Control.Arrow (second)

checkLaws :: Set a -> [(law, Property a)] -> Maybe (law, [a])
checkLaws d laws =
  let results = [ (law, checkProperty (Set.toList d) p) | (law, p) <- laws]
      f (law, res) = case res of
        PropHolds -> Nothing
        PropFailed as -> Just (law, as)
  in listToMaybe (mapMaybe f results) -- Return the first law that fails.

class Algebra alg a law | alg -> a, alg -> law where
  -- | The underlying set of the algebra.
  algebraSet :: alg -> Set a

  -- | The laws that must hold for the algebra.
  algebraLaws :: [(law, alg -> Property a)]

-- | Check all the laws hold for a given algebra. If a law does not hold, return
-- @Just (law, inputs)@, where @law@ is the law that fails, and @inputs@ are the
-- inputs to the law that constitute a counterexample.
checkAlgebra :: Algebra alg a law => alg -> Maybe (law, [a])
checkAlgebra alg = checkLaws (algebraSet alg) (second ($ alg) <$> algebraLaws)

class (Algebra alg a law, Algebra alg' b law) => Morphism m morphismLaw alg alg' a b law | m -> alg, m -> alg', m -> morphismLaw where
  morphismDomain   :: m -> alg
  morphismCodomain :: m -> alg'
  morphism         :: m -> a -> b
  morphismLaws     :: [(morphismLaw, m -> Property a)]

checkMorphism :: Morphism m morphismLaw alg alg' a b law => m -> Maybe (morphismLaw, [a])
checkMorphism m = checkLaws (algebraSet (morphismDomain m)) (second ($ m) <$> morphismLaws)

morphismTable :: Morphism m morphismLaw alg alg' a b law => m -> [(a, b)]
morphismTable m = [ (a, morphism m a)
                  | a <- Set.toList (algebraSet (morphismDomain m))
                  ]
