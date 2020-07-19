{-|
Description      : Traits associated with values
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com

Traits are optional attributes that a value might posess. For example,
a list containing totally ordered values might have a @Maximum@ trait
where the associated attribute is the maximum value. The trait exists
only if the list is non-empty.

Traits help to access these attributes in a type-safe manner.

Traits are somewhat similar to [refinement
types](https://hackage.haskell.org/package/refined), but allow
arbitrary attributes to be associated with a value instead of only a
predicate.
-}
module WebGear.Trait
  ( -- * Core Types
    Trait (..)
  , Traits
  , Linked

    -- * Linking values with traits
  , linkzero
  , linkone
  , linkplus
  , linkminus
  , unlink

    -- * Retrive trait attributes from linked values
  , Has (..)
  , Have
  ) where


--------------------------------------------------------------------------------
-- | A 'Trait' is an optional attribute @t@ associated with a value @a@.
--
-- The 'check' function validates the presence of the trait for a given value.
class Monad m => Trait t a m where
  -- | Type of the associated attribute
  type Val t a

  -- | Checks the presence of the associated attribute.
  check :: Tagged t a              -- ^ The value to check tagged with
                                   -- the trait
        -> m (Maybe (a, Val t a))  -- ^ Returns 'Nothing' if the trait
                                   -- is not present, a 'Just' value
                                   -- otherwise. Checking presence of
                                   -- a trait can optionally update
                                   -- the value as well

-- | A trivial trait that is always present and whose attribute does not carry
-- any meaningful information.
instance Monad m => Trait '[] a m where
  type Val '[] a = ()

  check :: Tagged '[] a -> m (Maybe (a, ()))
  check a = pure $ Just (untag a, ())

-- | Combination of many traits all of which are present for a value.
instance (Trait t a m, Trait ts a m) => Trait (t : ts :: [k]) a m where
  type Val (t : ts) a = (Val t a, Val ts a)

  check :: Tagged (t : ts) a -> m (Maybe (a, Val (t:ts) a))
  check (Tagged a) = check (Tagged @t a) >>= \case
    Nothing      -> pure Nothing
    Just (a', l) -> check (Tagged @ts a') >>= \case
      Nothing       -> pure Nothing
      Just (a'', r) -> pure $ Just (a'', (l, r))

-- | Constraint for functions that use multiple traits
type family Traits (ts :: [k]) a m :: Constraint where
  Traits '[]    a m = ()
  Traits (t:ts) a m = (Trait t a m, Traits ts a m)


--------------------------------------------------------------------------------
-- | A value linked with a trait attribute
data Linked ts a = Linked
    { linkVal :: !(Val ts a)
    , unlink  :: a            -- ^ Retrive the value from a linked value
    }

-- | Link a value with the trivial trait
linkzero :: a -> Linked '[] a
linkzero = Linked ()

-- | Attempt to link a value with a single trait
linkone :: Trait t a m => a -> m (Maybe (Linked '[t] a))
linkone = linkplus . linkzero

-- | Attempt to link an additional trait with an already linked value
linkplus :: Trait t a m => Linked ts a -> m (Maybe (Linked (t:ts) a))
linkplus l = fix $ \result -> do
  v <- check (toTagged result (unlink l))
  pure $ mkLinked v l
  where
    toTagged :: m (Maybe (Linked (t:ts) a)) -> a -> Tagged t a
    toTagged = const Tagged

    mkLinked :: Maybe (a, Val t a) -> Linked ts a -> Maybe (Linked (t:ts) a)
    mkLinked v lv = (\(a', left) -> Linked (left, linkVal lv) a') <$> v

-- | Remove the leading trait from the linked value
linkminus :: Linked (t:ts) a -> Linked ts a
linkminus l = Linked (snd $ linkVal l) (unlink l)


--------------------------------------------------------------------------------
-- | Constraint that proves that the trait @t@ is present somewhere in the list
-- of traits @ts@.
class Has t ts where
  traitValue :: Linked ts a -> Tagged t (Val t a)

instance Has t (t:ts) where
  traitValue :: Linked (t : ts) a -> Tagged t (Val t a)
  traitValue (Linked (lv, _) _) = Tagged lv

instance {-# OVERLAPPABLE #-} Has t ts => Has t (t':ts) where
  traitValue :: Linked (t':ts) a -> Tagged t (Val t a)
  traitValue l = traitValue (rightLinked l)
    where
      rightLinked :: Linked (q:qs) b -> Linked qs b
      rightLinked (Linked (_, rv) a) = Linked rv a


-- | Constraint that proves that all the traits in @ts@ are present
-- in @qs@
type family Have ts qs :: Constraint where
  Have '[]    qs = ()
  Have (t:ts) qs = (Has t qs, Have ts qs)
