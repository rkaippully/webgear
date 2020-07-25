{-|
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com

Middlewares related to HTTP headers.
-}
module WebGear.Middlewares.Header
  ( requestContentType
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (fromList)
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol)
import Network.HTTP.Types (badRequest400)
import Text.Printf (printf)

import WebGear.Route (MonadRouter (..))
import WebGear.Trait (linkplus)
import WebGear.Trait.Header (HeaderMatch, HeaderMismatch (..))
import WebGear.Types (RequestMiddleware, Response (..))


-- | A middleware to check that the Content-Type header in the request
-- has a specific value. It will fail the handler if the header did
-- not match.
--
-- Typical usage:
--
-- > requestContentType @"application/json" handler
--
requestContentType :: forall c m req res a. (KnownSymbol c, MonadRouter m)
                   => RequestMiddleware m req (HeaderMatch "Content-Type" c:req) res a
requestContentType handler = Kleisli $
  linkplus @(HeaderMatch "Content-Type" c) >=> either (failHandler . mkError) (runKleisli handler)
  where
    mkError :: HeaderMismatch -> Response ByteString
    mkError err = Response
                  { respStatus  = badRequest400
                  , respHeaders = fromList []
                  , respBody    = Just $ fromString $
                    case (expectedHeader err, actualHeader err) of
                      (ex, Nothing) -> printf "Expected Content-Type header %s but not found" (show ex)
                      (ex, Just h)  -> printf "Expected Content-Type header %s but found %s" (show ex) (show h)
                  }
