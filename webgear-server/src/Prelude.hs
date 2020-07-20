module Prelude
  ( module Relude
  , module Data.Semigroup
  , module GHC.TypeLits
  , module Data.List
  , module Data.Tagged
  , takeWhileM
  , splitOn
  ) where

import Data.List (groupBy, stripPrefix)
import Data.Semigroup (First (..))
import Data.Tagged
import GHC.TypeLits hiding (natVal, someNatVal)
import Relude hiding (First (..))


takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM _    []     = pure []
takeWhileM p (mx:mxs) = do
  x <- mx
  if p x
    then (x :) <$> takeWhileM p mxs
    else pure []

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr f ([] :| [])
  where
    f x acc       | x == sep = [] :| toList acc
    f x (y :| ys) = (x:y) :| ys
