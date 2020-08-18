{-# LANGUAGE UndecidableInstances #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Traits are optional attributes associated with a value. For
-- example, a list containing totally ordered values might have a
-- @Maximum@ trait where the associated attribute is the maximum
-- value. The trait exists only if the list is non-empty.
--
-- Traits help to access these attributes in a type-safe manner.
--
-- Traits are somewhat similar to [refinement
-- types](https://hackage.haskell.org/package/refined), but allow
-- arbitrary attributes to be associated with a value instead of only
-- a predicate.
module WebGear.Trait
  ( -- * Core Types
    Trait (..)
  , Result (..)
  , Linked
  , Traits

    -- * Linking values with traits
  , linkzero
  , linkplus
  , linkminus
  , unlink

    -- * Retrive trait attributes from linked values
  , Has (..)
  , Have
  ) where

import Data.Kind (Constraint, Type)
import Data.Tagged (Tagged (..))


-- | A 'Trait' is an optional attribute @t@ associated with a value
-- @a@.
class Monad m => Trait t a m where
  -- | Type of the associated attribute when the trait holds for a
  -- value
  type Attribute t a

  -- | Type that indicates that the trait does not hold for a
  -- value. This could be an error message, parse error etc.
  type Absence t a

  -- | Check whether the trait holds for the value @a@.
  prove :: a -> m (Result t a)

-- | Result of a 'prove' operation
data Result t a = Proof a (Attribute t a)
                | Refutation (Absence t a)

deriving instance (Eq a, Eq (Attribute t a), Eq (Absence t a)) => Eq (Result t a)
deriving instance (Show a, Show (Attribute t a), Show (Absence t a)) => Show (Result t a)
deriving instance (Read a, Read (Attribute t a), Read (Absence t a)) => Read (Result t a)


-- | A trivial trait that is always present and whose attribute does
-- not carry any meaningful information.
instance Monad m => Trait '[] a m where
  type Attribute '[] a = ()
  type Absence '[] a = ()

  prove :: a -> m (Result '[] a)
  prove a = pure $ Proof a ()

-- | Combination of many traits all of which are present for a value.
instance (Trait t a m, Trait ts a m) => Trait (t:ts) a m where
  type Attribute (t:ts) a = (Attribute t a, Attribute ts a)
  type Absence (t:ts) a = Either (Result t a) (Result ts a)

  prove :: a -> m (Result (t:ts) a)
  prove a = prove @t a >>= \case
    e@(Refutation _)   -> pure $ Refutation $ Left e
    Proof a' l -> prove @ts a' >>= \case
      e@(Refutation _)    -> pure $ Refutation $ Right e
      Proof a'' r -> pure $ Proof a'' (l, r)

-- | Constraint for functions that use multiple traits
type family Traits ts a m :: Constraint where
  Traits '[]    a m = ()
  Traits (t:ts) a m = (Trait t a m, Traits ts a m)

-- | A value linked with a type-level list of traits.
data Linked (ts :: [Type]) a = Linked
    { linkVal :: !(Attribute ts a)
    , unlink  :: !a                 -- ^ Retrive the value from a linked value
    }

-- | Link a value with the trivial trait
linkzero :: a -> Linked '[] a
linkzero = Linked ()

-- | Attempt to link an additional trait with an already linked value
linkplus :: Trait t a m => Linked ts a -> m (Either (Absence t a) (Linked (t:ts) a))
linkplus l = do
  v <- prove (unlink l)
  pure $ mkLinked v l
  where
    mkLinked :: Result t a -> Linked ts a -> Either (Absence t a) (Linked (t:ts) a)
    mkLinked (Proof a left) lv = Right $ Linked (left, linkVal lv) a
    mkLinked (Refutation e) _  = Left e

-- | Remove the leading trait from the linked value
linkminus :: Linked (t:ts) a -> Linked ts a
linkminus l = Linked (snd $ linkVal l) (unlink l)


-- | Constraint that proves that the trait @t@ is present in the list
-- of traits @ts@.
class Has t ts where
  -- | Get the attribute associated with @t@ from a linked value
  trait :: Linked ts a -> Tagged t (Attribute t a)

instance Has t (t:ts) where
  trait :: Linked (t:ts) a -> Tagged t (Attribute t a)
  trait (Linked (lv, _) _) = Tagged lv

instance {-# OVERLAPPABLE #-} Has t ts => Has t (t':ts) where
  trait :: Linked (t':ts) a -> Tagged t (Attribute t a)
  trait l = trait (rightLinked l)
    where
      rightLinked :: Linked (q:qs) b -> Linked qs b
      rightLinked (Linked (_, rv) a) = Linked rv a


-- | Constraint that proves that all the traits in the list @ts@ are
-- also present in the list @qs@.
type family Have ts qs :: Constraint where
  Have '[]    qs = ()
  Have (t:ts) qs = (Has t qs, Have ts qs)
