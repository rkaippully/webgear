module WebGear.Middleware
  ( method
  , path
  , pathVar
  , match
  , requestContentType
  , jsonRequestBody
  , jsonResponseBody
  ) where

import Control.Arrow
import Data.Aeson (FromJSON, ToJSON)

import WebGear.Trait
import WebGear.Trait.Body
import WebGear.Trait.Header
import WebGear.Trait.Method
import WebGear.Trait.Path
import WebGear.Types

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Web.HttpApiData as HTTP


routingMiddleware :: forall t ts res m a. (Trait t Request m, MonadPlus m) => RequestMiddleware m ts (t:ts) res a
routingMiddleware handler = Kleisli $ linkplus @t >=> maybe mzero (runKleisli handler)

method :: (IsStdMethod t, MonadPlus m) => RequestMiddleware m ts (Method t:ts) res a
method = routingMiddleware

path :: forall s ts res m a. (KnownSymbol s, MonadRouter m)
     => RequestMiddleware m ts (Path s:ts) res a
path = routingMiddleware

pathVar :: forall tag val ts res m a. (HTTP.FromHttpApiData val, MonadRouter m)
        => RequestMiddleware m ts (PathVar tag val:ts) res a
pathVar = routingMiddleware

match :: TH.QuasiQuoter
match = TH.QuasiQuoter
  { quoteExp  = toExp
  , quotePat  = const $ fail "You cannot use match in a pattern"
  , quoteType = const $ fail "You cannot use match in a type"
  , quoteDec  = const $ fail "You cannot use match in a declaration"
  }
  where
    toExp :: String -> TH.Q TH.Exp
    toExp s = case List.words s of
      [m, p] -> do
        let methodExp = TH.AppTypeE (TH.VarE 'method) (TH.ConT $ TH.mkName m)
        pathExps <- toPathExps p
        pure $ List.foldr1 compose $ methodExp :| toList pathExps
      [p]    -> do
        pathExps <- toPathExps p
        pure $ List.foldr1 compose $ toList pathExps
      _      -> fail "match expects an HTTP method and a path or just a path"

    toPathExps :: String -> TH.Q [TH.Exp]
    toPathExps p = splitOn '/' p
                   & fmap (splitOn ':')
                   & List.foldr joinPath []
                   & fmap toPathExp
                   & sequence

    joinPath :: NonEmpty String -> [NonEmpty String] -> [NonEmpty String]
    joinPath s []                    = [s]
    joinPath (s:|[]) ((s':|[]) : xs) = one (s <> "/" <> s') : xs
    joinPath y (x:xs)                = y:x:xs

    toPathExp :: NonEmpty String -> TH.Q TH.Exp
    toPathExp (p :| [])  = pure $ TH.AppTypeE (TH.VarE 'path) (TH.LitT $ TH.StrTyLit p)
    toPathExp (v :| [t]) = pure $ TH.AppTypeE (TH.AppTypeE (TH.VarE 'pathVar) (TH.LitT $ TH.StrTyLit v)) (TH.ConT $ TH.mkName t)
    toPathExp xs         = fail $ "Invalid path component: " <> intercalate ":" (toList xs)

    compose :: TH.Exp -> TH.Exp -> TH.Exp
    compose l = TH.UInfixE l (TH.VarE $ TH.mkName ".")


exceptMiddleware :: forall t ts res m a. (Trait t Request m, MonadRouter m) => Wai.Response -> RequestMiddleware m ts (t:ts) res a
exceptMiddleware err handler = Kleisli $ linkplus @t >=> maybe (throwError $ Just $ First err) (runKleisli handler)

requestContentType :: (KnownSymbol c, MonadRouter m) => RequestMiddleware m ts (HasContentType c:ts) res a
requestContentType = exceptMiddleware $ Wai.responseLBS HTTP.badRequest400 [] "Invalid Content-type"

jsonRequestBody :: (FromJSON t, MonadIO m, MonadRouter m) => RequestMiddleware m ts (JSONRequestBody t:ts) res a
jsonRequestBody = exceptMiddleware $ Wai.responseLBS HTTP.badRequest400 [] "Could not parse JSON body"

jsonResponseBody :: (ToJSON t, Monad m) => Middleware m req req res '[] t LByteString
jsonResponseBody handler = Kleisli $ \req -> do
  x <- unlink <$> runKleisli handler req
  pure $ linkzero $ Response
    { respStatus  = respStatus x
    , respHeaders = HM.insert HTTP.hContentType "application/json" $ respHeaders x
    , respBody    = Aeson.encode <$> respBody x
    }
