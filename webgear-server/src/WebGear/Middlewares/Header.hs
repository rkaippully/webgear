module WebGear.Middlewares.Header
  ( requestContentType
  ) where

import Control.Arrow (Kleisli (..))
import Network.HTTP.Types (badRequest400)

import WebGear.Route (MonadRouter (..))
import WebGear.Trait (linkplus)
import WebGear.Trait.Header (HasContentType)
import WebGear.Types (RequestMiddleware, Response (..))


requestContentType :: forall c m req res a. (KnownSymbol c, MonadRouter m)
                   => RequestMiddleware m req (HasContentType c:req) res a
requestContentType handler = Kleisli $ linkplus @(HasContentType c) >=> maybe (failHandler err) (runKleisli handler)
  where
    err :: Response LByteString
    err = Response
          { respStatus  = badRequest400
          , respHeaders = fromList []
          , respBody    = Just "Invalid Content-type"
          }
