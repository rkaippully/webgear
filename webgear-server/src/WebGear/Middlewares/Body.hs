-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to HTTP body.
module WebGear.Middlewares.Body
  ( JSONRequestBody
  , jsonRequestBody
  , jsonResponseBody
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode', encode)
import Data.ByteString.Lazy (ByteString, fromChunks, fromStrict)
import Data.Kind (Type)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (hContentType)

import WebGear.Trait (Result (..), Trait (..), probe)
import WebGear.Types (MonadRouter (..), Request, RequestMiddleware', Response (..),
                      ResponseMiddleware', badRequest400, getRequestBodyChunk, setResponseHeader)
import WebGear.Util (takeWhileM)

-- | A 'Trait' for converting a JSON request body into a value.
data JSONRequestBody (t :: Type)

instance (FromJSON t, MonadIO m) => Trait (JSONRequestBody t) Request m where
  type Attribute (JSONRequestBody t) Request = t
  type Absence (JSONRequestBody t) Request = Text

  toAttribute :: Request -> m (Result (JSONRequestBody t) Request)
  toAttribute r = do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ getRequestBodyChunk r
    pure $ case eitherDecode' (fromChunks chunks) of
             Left e  -> NotFound (pack e)
             Right t -> Found t

-- | A middleware to parse the request body as JSON and convert it to
-- a value via a 'FromJSON' instance.
--
-- Usage for a type @t@ which has a 'FromJSON' instance:
--
-- > jsonRequestBody @t handler
--
-- Returns a 400 Bad Request response on failure to parse body.
jsonRequestBody :: forall t m req a. (FromJSON t, MonadRouter m, MonadIO m)
                => RequestMiddleware' m req (JSONRequestBody t:req) a
jsonRequestBody handler = Kleisli $
  probe @(JSONRequestBody t) >=> either (errorResponse . mkError) (runKleisli handler)
  where
    mkError :: Text -> Response ByteString
    mkError e = badRequest400 $ fromStrict $ encodeUtf8 $ "Error parsing request body: " <> e

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
jsonResponseBody :: (ToJSON t, Monad m) => ResponseMiddleware' m req t ByteString
jsonResponseBody handler = Kleisli $ \req -> do
  x <- runKleisli handler req
  pure $ setResponseHeader hContentType "application/json" $ encode <$> x
