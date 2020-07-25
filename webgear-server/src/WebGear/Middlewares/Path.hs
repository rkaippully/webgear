{-|
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com

Middlewares related to route paths.
-}
module WebGear.Middlewares.Path
  ( path
  , pathVar
  , match
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..), toList)
import GHC.TypeLits (KnownSymbol)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Q, TyLit (..), Type (..), mkName)
import Web.HttpApiData (FromHttpApiData)

import WebGear.Middlewares.Method (method)
import WebGear.Route (MonadRouter (..))
import WebGear.Trait (linkplus)
import WebGear.Trait.Path (Path, PathVar)
import WebGear.Types (RequestMiddleware)
import WebGear.Util (splitOn)

import qualified Data.List as List


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
path :: forall s ts res m a. (KnownSymbol s, MonadRouter m)
     => RequestMiddleware m ts (Path s:ts) res a
path handler = Kleisli $
  linkplus @(Path s) >=> either (const rejectRoute) (runKleisli handler)

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
pathVar :: forall tag val ts res m a. (FromHttpApiData val, MonadRouter m)
        => RequestMiddleware m ts (PathVar tag val:ts) res a
pathVar handler = Kleisli $
  linkplus @(PathVar tag val) >=> either (const rejectRoute) (runKleisli handler)

-- | Produces middleware(s) to match an optional HTTP method and path.
--
-- This quasiquoter can be used in several ways:
--
-- * @[match|a\/b\/c]@ is equivalent to @path \@\"a\/b\/c\"@
-- * @[match|a\/b\/objId:Int\/d]@ is equivalent to
--   @path \@\"a\/b\" . pathVar \@\"objId\" \@Int . path @\"d\"@
-- * @[match|GET a\/b\/c]@ is equivalent to
--   @method \@GET $ path \@\"a\/b\/c\"@
-- * @[match|GET a\/b\/objId:Int\/d]@ is equivalent to
--   @method \@GET . path \@\"a\/b\" . pathVar \@\"objId\" \@Int . path \@\"d\"@
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
