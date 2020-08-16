-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to HTTP body.
module WebGear.Middlewares.Body
  ( jsonRequestBody
  , jsonResponseBody
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.HashMap.Strict (fromList, insert)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (badRequest400, hContentType)

import WebGear.Route (MonadRouter (..))
import WebGear.Trait (linkplus, linkzero, unlink)
import WebGear.Trait.Body (JSONRequestBody)
import WebGear.Types (Middleware, RequestMiddleware, Response (..))


-- | A middleware to parse the request body as JSON and convert it to
-- a value via a 'FromJSON' instance.
--
-- Usage for a type @t@ which has a 'FromJSON' instance:
--
-- > jsonRequestBody @t handler
--
jsonRequestBody :: forall t m req res a. (FromJSON t, MonadRouter m, MonadIO m)
                => RequestMiddleware m req (JSONRequestBody t:req) res a
jsonRequestBody handler = Kleisli $
  linkplus @(JSONRequestBody t) >=> either (failHandler . mkError) (runKleisli handler)
  where
    mkError :: Text -> Response ByteString
    mkError e = Response
          { respStatus  = badRequest400
          , respHeaders = fromList []
          , respBody    = Just $ fromStrict $ encodeUtf8 $ "Error parsing request body: " <> e
          }

-- | A middleware that converts the response that has a 'ToJSON'
-- instance to a 'ByteString' response.
--
-- This will also set the "Content-Type" header of the response to
-- "application/json".
--
-- Usage for a type @t@ which has a 'ToJSON' instance:
--
-- > jsonResponseBody @t handler
--
jsonResponseBody :: (ToJSON t, Monad m) => Middleware m req req res '[] t ByteString
jsonResponseBody handler = Kleisli $ \req -> do
  x <- unlink <$> runKleisli handler req
  pure $ linkzero $ Response
    { respStatus  = respStatus x
    , respHeaders = insert hContentType "application/json" $ respHeaders x
    , respBody    = encode <$> respBody x
    }
