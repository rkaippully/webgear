{-# LANGUAGE UndecidableInstances #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
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
  , Linked

    -- * Linking values with attributes
  , linkzero
  , unlink
  , probe
  , transcribe

    -- * Retrive trait attributes from linked values
  , Has (..)
  , Have

  , MissingTrait
  ) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)


-- | A trait is an optional attribute @t@ associated with a value
-- @a@.
class Monad m => Trait (t :: Type) (ts :: [Type]) a m where
  -- | Type of the associated attribute when the trait holds for a
  -- value
  type Attribute t a :: Type

  -- | Type that indicates that the trait does not exist for a
  -- value. This could be an error message, parse error etc.
  type Absence t a :: Type

  -- | Attempt to deduce the trait attribute from the value @a@.
  tryLink :: t -> Linked ts a -> m (Either (Absence t a) (Attribute t a))


type family LinkedAttributes (ts :: [Type]) (a :: Type) where
  LinkedAttributes '[] a = ()
  LinkedAttributes (t:ts) a = (Attribute t a, LinkedAttributes ts a)

-- | A value linked with a type-level list of traits.
data Linked (ts :: [Type]) a = Linked
    { linkAttribute :: !(LinkedAttributes ts a)
    , unlink        :: !a  -- ^ Retrive the value from a linked value
    }

-- | Wrap a value with an empty list of traits.
linkzero :: a -> Linked '[] a
linkzero = Linked ()

-- | Attempt to link an additional trait with an already linked value
-- via the 'toAttribute' operation. This can fail indicating an
-- 'Absence' of the trait.
probe :: forall t ts a m. Trait t ts a m
      => t
      -> Linked ts a
      -> m (Either (Absence t a) (Linked (t:ts) a))
probe t l@Linked{..} = fmap link <$> tryLink t l
  where
    link :: Attribute t a -> Linked (t:ts) a
    link attr = Linked {linkAttribute = (attr, linkAttribute), ..}

-- | Reencode one trait to another.
--
-- Like 'probe', but instead of adding a new trait to the trait list,
-- uses the first trait in the list to probe the presence of a new
-- trait and replaces the old trait with the new one.
transcribe :: forall t2 t1 ts a m. Trait t2 (t1:ts) a m
           => t2
           -> Linked (t1:ts) a
           -> m (Either (Absence t2 a) (Linked (t2:ts) a))
transcribe t2 l@Linked{..} = fmap link <$> tryLink t2 l
  where
    link :: Attribute t2 a -> Linked (t2:ts) a
    link attr = Linked {linkAttribute = (attr, snd linkAttribute), ..}


-- | Constraint that proves that the trait @t@ is present in the list
-- of traits @ts@.
class Has t ts where
  -- | Get the attribute associated with @t@ from a linked value
  get :: Proxy t -> Linked ts a -> Attribute t a

instance Has t (t:ts) where
  get :: Proxy t -> Linked (t:ts) a -> Attribute t a
  get _ (Linked (lv, _) _) = lv

instance {-# OVERLAPPABLE #-} Has t ts => Has t (t':ts) where
  get :: Proxy t -> Linked (t':ts) a -> Attribute t a
  get _ l = get (Proxy @t) (rightLinked l)
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
                      :$$: Text "For e.g., ‘QueryParam \"foo\" Int’ instead of ‘QueryParam \"foo\" String’?"
                      :$$: Text ""
                      :$$: Text "Or did you forget to apply an appropriate middleware?"
                      :$$: Text "For e.g. The trait ‘JSONRequestBody Foo’ can be used with ‘jsonRequestBody @Foo’ middleware."
                      :$$: Text ""


-- | Constraint that proves that all the traits in the list @ts@ are
-- also present in the list @qs@.
type family Have ts qs :: Constraint where
  Have '[]    qs = ()
  Have (t:ts) qs = (Has t qs, Have ts qs)
