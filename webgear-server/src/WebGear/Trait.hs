{-# LANGUAGE UndecidableInstances #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Traits are optional attributes associated with a value. For
-- example, a list containing totally ordered values might have a
-- @Maximum@ trait where the associated attribute is the maximum
-- value. This trait exists only if the list is non-empty.
--
-- It is also possible for some traits to be explicitly attached with
-- a value. For example, we can have a @Head@ trait for a list which
-- can be established by the @cons@ operation. Not all traits will
-- support this 'attach' operation.
--
-- Traits help to link attributes with values in a type-safe manner.
--
-- Traits are somewhat similar to [refinement
-- types](https://hackage.haskell.org/package/refined), but allow
-- arbitrary attributes to be associated with a value instead of only
-- a predicate.
--
module WebGear.Trait
  ( -- * Core Types
    Trait (..)
  , Result (..)
  , Attachable (..)
  , Linked

    -- * Linking values with attributes
  , link
  , unlink
  , probe
  , connect
  , remove

    -- * Retrive trait attributes from linked values
  , Has (..)
  , Have
  ) where

import Data.Kind (Constraint, Type)
import Data.Tagged (Tagged (..), untag)


-- | A trait is an optional attribute @t@ associated with a value
-- @a@. Its presence can be deduced via the 'derive' function.
class Monad m => Trait t a m where
  -- | Type of the associated attribute when the trait holds for a
  -- value
  type Attribute t a

  -- | Type that indicates that the trait does not exist for a
  -- value. This could be an error message, parse error etc.
  type Absence t a

  -- | Attempt to deduce the trait attribute from the value @a@.
  derive :: a -> m (Result t a)

-- | The result of 'derive' - either a successful deduction of an
-- attribute with a potentially updated value, or an error.
data Result t a = Refutation (Absence t a)
                | Proof a (Attribute t a)


-- | A trait that allows explicitly attaching the trait attribute with
-- a value.
class Trait t a m => Attachable t a m where
  -- | Associate a trait attribute with a value.
  attach :: Attribute t a -> a -> m (Tagged t a)


-- | A trivial derivable trait that is always present and whose
-- attribute does not carry any meaningful information.
instance Monad m => Trait '[] a m where
  type Attribute '[] a = ()
  type Absence '[] a = ()

  derive :: a -> m (Result '[] a)
  derive a = pure $ Proof a ()

instance Monad m => Attachable '[] a m where
  attach :: () -> a -> m (Tagged '[] a)
  attach _ = pure . Tagged

-- | Combination of many derivable traits all of which are present for
-- a value.
instance (Trait t a m, Trait ts a m) => Trait (t:ts) a m where
  type Attribute (t:ts) a = (Attribute t a, Attribute ts a)
  type Absence (t:ts) a = Either (Result t a) (Result ts a)

  derive :: a -> m (Result (t:ts) a)
  derive a = derive @t a >>= \case
    e@(Refutation _) -> pure $ Refutation $ Left e
    Proof a' l       -> derive @ts a' >>= \case
      e@(Refutation _) -> pure $ Refutation $ Right e
      Proof a'' r      -> pure $ Proof a'' (l, r)

instance (Attachable t a m, Attachable ts a m) => Attachable (t:ts) a m where
  attach :: (Attribute t a, Attribute ts a) -> a -> m (Tagged (t:ts) a)
  attach (attr, attrs) a = attach @ts attrs a >>=
    \(Tagged a') -> Tagged . untag <$> attach @t attr a'


-- | A value linked with a type-level list of traits.
data Linked (ts :: [Type]) a = Linked
    { linkAttribute :: !(Attribute ts a)
    , unlink        :: !a                 -- ^ Retrive the value from a linked value
    }

-- | Wrap a value with an empty list of traits.
link :: a -> Linked '[] a
link = Linked ()

-- | Attempt to link an additional trait with an already linked value
-- via a 'derive' operation. This can fail indicating an 'Absence' of
-- the trait.
probe :: Trait t a m => Linked ts a -> m (Either (Absence t a) (Linked (t:ts) a))
probe l = do
  v <- derive (unlink l)
  pure $ mkLinked v l
  where
    mkLinked :: Result t a -> Linked ts a -> Either (Absence t a) (Linked (t:ts) a)
    mkLinked (Proof a left) lv = Right $ Linked (left, linkAttribute lv) a
    mkLinked (Refutation e) _  = Left e

-- | Given a linked value and an attribute value for an attachable
-- trait, connect the trait with the linked value using the 'attach'
-- operation for the trait.
connect :: forall t ts a m. Attachable t a m
        => Attribute t a
        -> Linked ts a
        -> m (Linked (t:ts) a)
connect attr l = mkLinked <$> attach @t attr (unlink l)
  where
    mkLinked :: Tagged t a -> Linked (t:ts) a
    mkLinked (Tagged a) = Linked (attr, linkAttribute l) a

-- | Remove the leading trait from the type-level list of traits
remove :: Linked (t:ts) a -> Linked ts a
remove l = Linked (snd $ linkAttribute l) (unlink l)


-- | Constraint that proves that the trait @t@ is present in the list
-- of traits @ts@.
class Has t ts where
  -- | Get the attribute associated with @t@ from a linked value
  get :: Linked ts a -> Tagged t (Attribute t a)

instance Has t (t:ts) where
  get :: Linked (t:ts) a -> Tagged t (Attribute t a)
  get (Linked (lv, _) _) = Tagged lv

instance {-# OVERLAPPABLE #-} Has t ts => Has t (t':ts) where
  get :: Linked (t':ts) a -> Tagged t (Attribute t a)
  get l = get (rightLinked l)
    where
      rightLinked :: Linked (q:qs) b -> Linked qs b
      rightLinked (Linked (_, rv) a) = Linked rv a

-- | Constraint that proves that all the traits in the list @ts@ are
-- also present in the list @qs@.
type family Have ts qs :: Constraint where
  Have '[]    qs = ()
  Have (t:ts) qs = (Has t qs, Have ts qs)
