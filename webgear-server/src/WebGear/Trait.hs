{-# LANGUAGE UndecidableInstances #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Traits are optional attributes associated with a value. For
-- example, a list containing totally ordered values might have a
-- @Maximum@ trait where the associated attribute is the maximum
-- value. This trait exists only if the list is non-empty. The 'Trait'
-- typeclass provides an interface to extract such trait attributes.
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
  , Linked

    -- * Linking values with attributes
  , link
  , unlink
  , probe
  , remove

    -- * Retrive trait attributes from linked values
  , Has (..)
  , Have

  , MissingTrait
  ) where

import Data.Kind (Constraint, Type)
import Data.Tagged (Tagged (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)


-- | A trait is an optional attribute @t@ associated with a value
-- @a@.
class Monad m => Trait t a m where
  -- | Type of the associated attribute when the trait holds for a
  -- value
  type Attribute t a :: Type

  -- | Type that indicates that the trait does not exist for a
  -- value. This could be an error message, parse error etc.
  type Absence t a :: Type

  -- | Attempt to deduce the trait attribute from the value @a@. It is
  -- possible that deducing a trait's presence can alter the value,
  -- hence this function returns a possibly updated value along with
  -- the trait attribute on success.
  toAttribute :: a -> m (Result t a)


-- | The result of 'toAttribute' - either a successful deduction of an
-- attribute with a potentially updated value, or an error.
data Result t a = Refutation (Absence t a)
                | Proof (Attribute t a)


-- | A trivial derivable trait that is always present and whose
-- attribute does not carry any meaningful information.
instance Monad m => Trait '[] a m where
  type Attribute '[] a = ()
  type Absence '[] a = ()

  toAttribute :: a -> m (Result '[] a)
  toAttribute = const $ pure $ Proof ()

-- | Combination of many derivable traits all of which are present for
-- a value.
instance (Trait t a m, Trait ts a m, Monad m) => Trait (t:ts) a m where
  type Attribute (t:ts) a = (Attribute t a, Attribute ts a)
  type Absence (t:ts) a = Either (Result t a) (Result ts a)

  toAttribute :: a -> m (Result (t:ts) a)
  toAttribute a = toAttribute @t a >>= \case
    e@(Refutation _) -> pure $ Refutation $ Left e
    Proof l          -> toAttribute @ts a >>= \case
      e@(Refutation _) -> pure $ Refutation $ Right e
      Proof r          -> pure $ Proof (l, r)


-- | A value linked with a type-level list of traits.
data Linked (ts :: [Type]) a = Linked
    { linkAttribute :: !(Attribute ts a)
    , unlink        :: !a                 -- ^ Retrive the value from a linked value
    }

-- | Wrap a value with an empty list of traits.
link :: a -> Linked '[] a
link = Linked ()

-- | Attempt to link an additional trait with an already linked value
-- via the 'toAttribute' operation. This can fail indicating an
-- 'Absence' of the trait.
probe :: forall t ts a m. Trait t a m
      => Linked ts a
      -> m (Either (Absence t a) (Linked (t:ts) a))
probe l = do
  v <- toAttribute @t (unlink l)
  pure $ mkLinked v l
  where
    mkLinked :: Result t a -> Linked ts a -> Either (Absence t a) (Linked (t:ts) a)
    mkLinked (Proof left) lv  = Right $ Linked (left, linkAttribute lv) (unlink lv)
    mkLinked (Refutation e) _ = Left e

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

-- For better type errors
instance TypeError (MissingTrait t) => Has t '[] where
   get = undefined

-- | Type error for nicer UX of missing traits
type MissingTrait t = Text "The request doesn't have the trait ‘" :<>: ShowType t :<>: Text "’."
                      :$$: Text ""
                      :$$: Text "Did you use a wrong trait type?"
                      :$$: Text "For e.g., ‘PathVar \"foo\" Int’ instead of ‘PathVar \"foo\" String’?"
                      :$$: Text ""
                      :$$: Text "Or did you forget to apply an appropriate middleware?"
                      :$$: Text "For e.g. The trait ‘JSONRequestBody Foo’ can be used with ‘jsonRequestBody @Foo’ middleware."
                      :$$: Text ""


-- | Constraint that proves that all the traits in the list @ts@ are
-- also present in the list @qs@.
type family Have ts qs :: Constraint where
  Have '[]    qs = ()
  Have (t:ts) qs = (Has t qs, Have ts qs)
