-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to route paths.
module WebGear.Middlewares.Path
  ( Path
  , PathVar
  , PathVarError (..)
  , path
  , pathVar
  , match
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Q, TyLit (..), Type (..), mkName)
import Web.HttpApiData (FromHttpApiData (..))

import WebGear.Middlewares.Method (method)
import WebGear.Route (MonadRouter (..))
import WebGear.Trait (Result (..), Trait (..), probe)
import WebGear.Types (Request, RequestMiddleware, pathInfo, setPathInfo)
import WebGear.Util (splitOn)

import qualified Data.List as List


-- | A path component which is literally matched against the request
-- but discarded after that.
data Path (s :: Symbol)

instance (KnownSymbol s, Monad m) => Trait (Path s) Request m where
  type Attribute (Path s) Request = ()
  type Absence (Path s) Request = ()

  toAttribute :: Request -> m (Result (Path s) Request)
  toAttribute r = pure $
    let expected = map pack $ toList $ splitOn '/' $ symbolVal $ Proxy @s
        actual = pathInfo r
    in
      case List.stripPrefix expected actual of
        Nothing   -> Refutation ()
        Just rest -> Proof (setPathInfo rest r) ()


-- | A path variable that is extracted and converted to a value of
-- type @val@. The @tag@ is usually a type-level symbol (string) to
-- uniquely identify this variable.
data PathVar tag (val :: *)

-- | Failure to extract a 'PathVar'
data PathVarError = PathVarNotFound | PathVarParseError Text
  deriving (Eq, Show, Read)

instance (FromHttpApiData val, Monad m) => Trait (PathVar tag val) Request m where
  type Attribute (PathVar tag val) Request = val
  type Absence (PathVar tag val) Request = PathVarError

  toAttribute :: Request -> m (Result (PathVar tag val) Request)
  toAttribute r = pure $
    case pathInfo r of
      []     -> Refutation PathVarNotFound
      (x:xs) ->
        case parseUrlPiece @val x of
          Left e  -> Refutation $ PathVarParseError e
          Right h -> Proof (setPathInfo xs r) h

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
     => RequestMiddleware m ts (Path s:ts) a
path handler = Kleisli $
  probe @(Path s) >=> either (const rejectRoute) (runKleisli handler)

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
        => RequestMiddleware m ts (PathVar tag val:ts) a
pathVar handler = Kleisli $
  probe @(PathVar tag val) >=> either (const rejectRoute) (runKleisli handler)

-- | Produces middleware(s) to match an optional HTTP method and path.
--
-- This quasiquoter can be used in several ways:
--
-- * @[match|a\/b\/c]@ is equivalent to @'path' \@\"a\/b\/c\"@
-- * @[match|a\/b\/objId:Int\/d]@ is equivalent to
--   @'path' \@\"a\/b\" . 'pathVar' \@\"objId\" \@Int . 'path' @\"d\"@
-- * @[match|GET a\/b\/c]@ is equivalent to
--   @'method' \@GET $ 'path' \@\"a\/b\/c\"@
-- * @[match|GET a\/b\/objId:Int\/d]@ is equivalent to
--   @'method' \@GET . 'path' \@\"a\/b\" . 'pathVar' \@\"objId\" \@Int . 'path' \@\"d\"@
--
match :: QuasiQuoter
match = QuasiQuoter
  { quoteExp  = toExp
  , quotePat  = const $ fail "match cannot be used in a pattern"
  , quoteType = const $ fail "match cannot be used in a type"
  , quoteDec  = const $ fail "match cannot be used in a declaration"
  }
  where
    toExp :: String -> Q Exp
    toExp s = case List.words s of
      [m, p] -> do
        let methodExp = AppTypeE (VarE 'method) (ConT $ mkName m)
        pathExps <- toPathExps p
        pure $ List.foldr1 compose $ methodExp :| pathExps
      [p]    -> do
        pathExps <- toPathExps p
        pure $ List.foldr1 compose pathExps
      _      -> fail "match expects an HTTP method and a path or just a path"

    toPathExps :: String -> Q [Exp]
    toPathExps p = splitOn '/' p
                   & fmap (splitOn ':')
                   & List.foldr joinPath []
                   & fmap toPathExp
                   & sequence

    joinPath :: NonEmpty String -> [NonEmpty String] -> [NonEmpty String]
    joinPath s []                    = [s]
    joinPath (s:|[]) ((s':|[]) : xs) = ((s <> "/" <> s') :| []) : xs
    joinPath y (x:xs)                = y:x:xs

    toPathExp :: NonEmpty String -> Q Exp
    toPathExp (p :| [])  = pure $ AppTypeE (VarE 'path) (LitT $ StrTyLit p)
    toPathExp (v :| [t]) = pure $ AppTypeE (AppTypeE (VarE 'pathVar) (LitT $ StrTyLit v)) (ConT $ mkName t)
    toPathExp xs         = fail $ "Invalid path component: " <> List.intercalate ":" (toList xs)

    compose :: Exp -> Exp -> Exp
    compose l = UInfixE l (VarE $ mkName ".")
