-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to route paths.
module WebGear.Middlewares.Path
  ( Path (..)
  , PathVar (..)
  , PathVarError (..)
  , PathEnd (..)
  , path
  , pathVar
  , pathEnd
  , match
  , route
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Control.Monad.State.Strict (MonadState (..))
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..), filter)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Q, TyLit (..), Type (..), mkName)
import Prelude hiding (drop, filter, take)
import Web.HttpApiData (FromHttpApiData (..))
import WebGear.Middlewares.Method (method)
import WebGear.Trait (Linked, Trait (..), probe)
import WebGear.Types (MonadRouter (..), PathInfo (..), Request, RequestMiddleware')
import WebGear.Util (splitOn)


-- | A path component which is literally matched against the request
-- but discarded after that.
data Path (s :: Symbol) = Path

instance (KnownSymbol s, MonadState PathInfo m) => Trait (Path s) ts Request m where
  type Attribute (Path s) Request = ()
  type Absence (Path s) Request = ()

  tryLink :: Path s
          -> Linked ts Request
          -> m (Either () ())
  tryLink _ _ = do
    PathInfo actualPath <- get
    case List.stripPrefix expectedPath actualPath of
      Nothing   -> pure $ Left ()
      Just rest -> do
        put $ PathInfo rest
        pure $ Right ()
    where
      expectedPath = Proxy @s
                     & symbolVal
                     & splitOn '/'
                     & filter (/= "")
                     & map pack


-- | A path variable that is extracted and converted to a value of
-- type @val@. The @tag@ is usually a type-level symbol (string) to
-- uniquely identify this variable.
data PathVar tag val = PathVar

-- | Failure to extract a 'PathVar'
data PathVarError = PathVarNotFound | PathVarParseError Text
  deriving (Eq, Show, Read)

instance (FromHttpApiData val, MonadState PathInfo m) => Trait (PathVar tag val) ts Request m where
  type Attribute (PathVar tag val) Request = val
  type Absence (PathVar tag val) Request = PathVarError

  tryLink :: PathVar tag val
          -> Linked ts Request
          -> m (Either PathVarError val)
  tryLink _ _ = do
    PathInfo actualPath <- get
    case actualPath of
      []     -> pure $ Left PathVarNotFound
      (x:xs) -> case parseUrlPiece @val x of
        Left e  -> pure $ Left $ PathVarParseError e
        Right v -> do
          put $ PathInfo xs
          pure $ Right v

-- | Trait to indicate that no more path components are present in the request
data PathEnd = PathEnd

instance MonadState PathInfo m => Trait PathEnd ts Request m where
  type Attribute PathEnd Request = ()
  type Absence PathEnd Request = ()

  tryLink :: PathEnd
          -> Linked ts Request
          -> m (Either () ())
  tryLink _ _ = do
    PathInfo actualPath <- get
    pure $ if null actualPath
           then Right ()
           else Left ()


-- | A middleware that literally matches path @s@.
--
-- The symbol @s@ could contain one or more parts separated by a
-- forward slash character. The route will be rejected if there is no
-- match.
--
-- For example, the following code could be used to match the URL path
-- \"a\/b\/c\" and then invoke @handler@:
--
-- > path @"a/b/c" handler
--
path :: forall s ts m a. (KnownSymbol s, MonadRouter m)
     => RequestMiddleware' m ts (Path s:ts) a
path handler = Kleisli $
  probe Path >=> either (const rejectRoute) (runKleisli handler)

-- | A middleware that captures a path variable from a single path
-- component.
--
-- The value captured is converted to a value of type @val@ via
-- 'FromHttpApiData'. The route will be rejected if the value is not
-- found or cannot be converted.
--
-- For example, the following code could be used to read a path
-- component as 'Int' tagged with the symbol \"objId\", and then
-- invoke @handler@:
--
-- > pathVar @"objId" @Int handler
--
pathVar :: forall tag val ts m a. (FromHttpApiData val, MonadRouter m)
        => RequestMiddleware' m ts (PathVar tag val:ts) a
pathVar handler = Kleisli $
  probe PathVar >=> either (const rejectRoute) (runKleisli handler)

-- | A middleware that verifies that end of path is reached.
pathEnd :: MonadRouter m => RequestMiddleware' m ts (PathEnd:ts) a
pathEnd handler = Kleisli $
  probe PathEnd >=> either (const rejectRoute) (runKleisli handler)

-- | Produces middleware(s) to match an optional HTTP method and some
-- path components.
--
-- This middleware matches a prefix of path components, the remaining
-- components can be matched by subsequent uses of 'match'.
--
-- This quasiquoter can be used in several ways:
--
-- +---------------------------------------+---------------------------------------------------------------------------------------+
-- | QuasiQuoter                           | Equivalent Middleware                                                                 |
-- +=======================================+=======================================================================================+
-- | @[match| \/a\/b\/c |]@                | @'path' \@\"\/a\/b\/c\"@                                                              |
-- +---------------------------------------+---------------------------------------------------------------------------------------+
-- | @[match| \/a\/b\/objId:Int\/d |]@     | @'path' \@\"\/a\/b\" . 'pathVar' \@\"objId\" \@Int . 'path' \@\"d\"@                  |
-- +---------------------------------------+---------------------------------------------------------------------------------------+
-- | @[match| GET \/a\/b\/c |]@            | @'method' \@GET . 'path' \@\"\/a\/b\/c\"@                                             |
-- +---------------------------------------+---------------------------------------------------------------------------------------+
-- | @[match| GET \/a\/b\/objId:Int\/d |]@ | @'method' \@GET . 'path' \@\"\/a\/b\" . 'pathVar' \@\"objId\" \@Int . 'path' \@\"d\"@ |
-- +---------------------------------------+---------------------------------------------------------------------------------------+
--
match :: QuasiQuoter
match = QuasiQuoter
  { quoteExp  = toMatchExp
  , quotePat  = const $ fail "match cannot be used in a pattern"
  , quoteType = const $ fail "match cannot be used in a type"
  , quoteDec  = const $ fail "match cannot be used in a declaration"
  }

-- | Produces middleware(s) to match an optional HTTP method and the
-- entire request path.
--
-- This middleware is intended to be used in cases where the entire
-- path needs to be matched. Use 'match' middleware to match only an
-- initial portion of the path.
--
-- This quasiquoter can be used in several ways:
--
-- +---------------------------------------+---------------------------------------------------------------------------------------------------+
-- | QuasiQuoter                           | Equivalent Middleware                                                                             |
-- +=======================================+===================================================================================================+
-- | @[route| \/a\/b\/c |]@                | @'path' \@\"\/a\/b\/c\" . 'pathEnd'@                                                              |
-- +---------------------------------------+---------------------------------------------------------------------------------------------------+
-- | @[route| \/a\/b\/objId:Int\/d |]@     | @'path' \@\"\/a\/b\" . 'pathVar' \@\"objId\" \@Int . 'path' \@\"d\" . 'pathEnd'@                  |
-- +---------------------------------------+---------------------------------------------------------------------------------------------------+
-- | @[route| GET \/a\/b\/c |]@            | @'method' \@GET . 'path' \@\"\/a\/b\/c\" . 'pathEnd'@                                             |
-- +---------------------------------------+---------------------------------------------------------------------------------------------------+
-- | @[route| GET \/a\/b\/objId:Int\/d |]@ | @'method' \@GET . 'path' \@\"\/a\/b\" . 'pathVar' \@\"objId\" \@Int . 'path' \@\"d\" . 'pathEnd'@ |
-- +---------------------------------------+---------------------------------------------------------------------------------------------------+
--
route :: QuasiQuoter
route = QuasiQuoter
  { quoteExp  = toRouteExp
  , quotePat  = const $ fail "route cannot be used in a pattern"
  , quoteType = const $ fail "route cannot be used in a type"
  , quoteDec  = const $ fail "route cannot be used in a declaration"
  }

toRouteExp :: String -> Q Exp
toRouteExp s = do
  e <- toMatchExp s
  pure $ compose e (VarE 'pathEnd)

toMatchExp :: String -> Q Exp
toMatchExp s = case List.words s of
  [m, p] -> do
    let methodExp = AppTypeE (VarE 'method) (ConT $ mkName m)
    pathExps <- toPathExps p
    pure $ List.foldr1 compose $ methodExp :| pathExps
  [p]    -> do
    pathExps <- toPathExps p
    pure $ List.foldr1 compose pathExps
  _      -> fail "Expected an HTTP method and a path or just a path"

  where
    toPathExps :: String -> Q [Exp]
    toPathExps p = splitOn '/' p
                   & filter (/= "")
                   & fmap (splitOn ':')
                   & List.foldr joinPath []
                   & mapM toPathExp

    joinPath :: NonEmpty String -> [NonEmpty String] -> [NonEmpty String]
    joinPath p []                    = [p]
    joinPath (p:|[]) ((p':|[]) : xs) = ((p <> "/" <> p') :| []) : xs
    joinPath y (x:xs)                = y:x:xs

    toPathExp :: NonEmpty String -> Q Exp
    toPathExp (p :| [])  = pure $ AppTypeE (VarE 'path) (LitT $ StrTyLit p)
    toPathExp (v :| [t]) = pure $ AppTypeE (AppTypeE (VarE 'pathVar) (LitT $ StrTyLit v)) (ConT $ mkName t)
    toPathExp xs         = fail $ "Invalid path component: " <> List.intercalate ":" (toList xs)

compose :: Exp -> Exp -> Exp
compose l = UInfixE l (VarE $ mkName ".")
