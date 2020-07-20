module WebGear.Middlewares.Path
  ( path
  , pathVar
  , match
  ) where

import Control.Arrow (Kleisli (..))
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (..), Q, TyLit (..), Type (..), mkName)
import Web.HttpApiData (FromHttpApiData)

import WebGear.Middlewares.Method (method)
import WebGear.Route (MonadRouter (..))
import WebGear.Trait (Trait, linkplus)
import WebGear.Trait.Path (Path, PathVar)
import WebGear.Types (Request, RequestMiddleware)

import qualified Data.List as List


-- | A middleware that checks for trait 't'; on failure it will reject this route
rejectingMiddleware :: forall req reqs res m a. (Trait req Request m, MonadRouter m)
                    => RequestMiddleware m reqs (req:reqs) res a
rejectingMiddleware handler = Kleisli $ linkplus @req >=> maybe rejectRoute (runKleisli handler)

path :: forall s ts res m a. (KnownSymbol s, MonadRouter m)
     => RequestMiddleware m ts (Path s:ts) res a
path = rejectingMiddleware

pathVar :: forall tag val ts res m a. (FromHttpApiData val, MonadRouter m)
        => RequestMiddleware m ts (PathVar tag val:ts) res a
pathVar = rejectingMiddleware

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
        pure $ List.foldr1 compose $ methodExp :| toList pathExps
      [p]    -> do
        pathExps <- toPathExps p
        pure $ List.foldr1 compose $ toList pathExps
      _      -> fail "match expects an HTTP method and a path or just a path"

    toPathExps :: String -> Q [Exp]
    toPathExps p = splitOn '/' p
                   & fmap (splitOn ':')
                   & List.foldr joinPath []
                   & fmap toPathExp
                   & sequence

    joinPath :: NonEmpty String -> [NonEmpty String] -> [NonEmpty String]
    joinPath s []                    = [s]
    joinPath (s:|[]) ((s':|[]) : xs) = one (s <> "/" <> s') : xs
    joinPath y (x:xs)                = y:x:xs

    toPathExp :: NonEmpty String -> Q Exp
    toPathExp (p :| [])  = pure $ AppTypeE (VarE 'path) (LitT $ StrTyLit p)
    toPathExp (v :| [t]) = pure $ AppTypeE (AppTypeE (VarE 'pathVar) (LitT $ StrTyLit v)) (ConT $ mkName t)
    toPathExp xs         = fail $ "Invalid path component: " <> intercalate ":" (toList xs)

    compose :: Exp -> Exp -> Exp
    compose l = UInfixE l (VarE $ mkName ".")
