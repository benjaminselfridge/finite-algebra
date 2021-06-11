{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}

-- | This module contains a type class, 'Checkable', for checking arbitrary
-- properties over algebraic operations (that is, operations that take 0 or more
-- inputs from a fixed, finite domain).
{-# LANGUAGE FlexibleContexts #-}
module Algebra.Finite.Property
  ( -- * Checkable
    Checkable(..)
  , PropCheckResult(..)
  , checkPassed
    -- * Property
  , Property(..)
  , checkProperty
  ) where

import Data.List (find)
import Data.Maybe (fromMaybe)

-- | The result of checking whether a property holds for all inputs from a
-- particular domain.
data PropCheckResult a = PropHolds
                       | PropFailed [a] -- ^ List of inputs for failure
  deriving Show

-- | Add another input to the front of a 'PropCheckResult'.
withInput :: a -> PropCheckResult a -> PropCheckResult a
withInput _ PropHolds = PropHolds
withInput a (PropFailed as) = PropFailed (a:as)

-- | Return 'True' if the check passed, 'False' if it failed.
checkPassed :: PropCheckResult a -> Bool
checkPassed (PropFailed _) = False
checkPassed PropHolds = True

-- | Type class for exhaustively checking a property against all inputs over
-- some domain.
--
-- For a given @a@, a type @p@ satisfying @Checkable p a@ will have the form
-- @Bool@, @a -> Bool@, @a -> a -> Bool@, etc.
--
-- >>> dom = [0..3::Integer]
-- >>> check True dom
-- PropHolds
-- >>> a # b = (a + b) `mod` (4::Integer)
-- >>> neg a = negate a `mod` (4 :: Integer)
-- >>> checkProperty dom (unaryClosed dom neg)
-- PropHolds
-- >>> checkProperty dom (unaryClosed dom negate)
-- PropFailed [1]
-- >>> checkProperty dom (binaryClosed dom (#))
-- PropHolds
-- >>> checkProperty dom (binaryClosed dom (+))
-- PropFailed [1,3]
-- >>> checkProperty dom (binaryAssoc (#))
-- PropHolds
-- >>> checkProperty dom (binaryAssoc (+))
-- PropHolds
class Checkable p a where
  check :: p -> [a] -> PropCheckResult a

instance Checkable Bool a where
  check True _ = PropHolds
  check False _ = PropFailed []

instance Checkable p a => Checkable (a -> p) a where
  check p as =
    let results = [ withInput a (check (p a) as) | a <- as ]
    in fromMaybe PropHolds (find (not . checkPassed) results)

-- | A 'Property' over a type @a@ is any boolean-valued function taking inputs
-- from @a@ which can be checked over all @a@ taken from a specified domain.
-- This is just a wrapper around 'Checkable'.
data Property a where
  Property :: Checkable p a => p -> Property a

-- | Check a property holds over a given domain.
checkProperty :: Property a -> [a] -> PropCheckResult a
checkProperty (Property p) = check p
