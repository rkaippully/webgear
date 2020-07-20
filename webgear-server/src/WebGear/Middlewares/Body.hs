module WebGear.Middlewares.Body
  ( jsonRequestBody
  , jsonResponseBody
  ) where

import Control.Arrow (Kleisli (..))

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.HashMap.Strict (insert)
import Network.HTTP.Types (badRequest400, hContentType)

import WebGear.Route (MonadRouter (..))
import WebGear.Trait (linkplus, linkzero, unlink)
import WebGear.Trait.Body (JSONRequestBody)
import WebGear.Types (Middleware, RequestMiddleware, Response (..))


jsonRequestBody :: forall t m req res a. (FromJSON t, MonadRouter m, MonadIO m)
                => RequestMiddleware m req (JSONRequestBody t:req) res a
jsonRequestBody handler = Kleisli $ linkplus @(JSONRequestBody t) >=> maybe (failHandler err) (runKleisli handler)
  where
    err :: Response LByteString
    err = Response
          { respStatus  = badRequest400
          , respHeaders = fromList []
          , respBody    = Just "Could not parse JSON body"
          }

jsonResponseBody :: (ToJSON t, Monad m) => Middleware m req req res '[] t LByteString
jsonResponseBody handler = Kleisli $ \req -> do
  x <- unlink <$> runKleisli handler req
  pure $ linkzero $ Response
    { respStatus  = respStatus x
    , respHeaders = insert hContentType "application/json" $ respHeaders x
    , respBody    = encode <$> respBody x
    }
