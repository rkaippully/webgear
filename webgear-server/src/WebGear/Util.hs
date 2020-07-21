module WebGear.Util
  ( takeWhileM
  , splitOn
  , rightToMaybe
  ) where

import Data.List.NonEmpty (NonEmpty (..), toList)


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

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just
