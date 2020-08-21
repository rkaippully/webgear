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
import Data.HashMap.Strict (fromList, insert)
import Data.Kind (Type)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (badRequest400, hContentType)

import WebGear.Route (MonadRouter (..))
import WebGear.Trait (Result (..), Trait (..), link, probe, unlink)
import WebGear.Types (Middleware, Request, RequestMiddleware, Response (..), getRequestBodyChunk)
import WebGear.Util (takeWhileM)

-- | A 'Trait' for converting a JSON request body into a value.
data JSONRequestBody (t :: Type)

instance (FromJSON t, MonadIO m) => Trait (JSONRequestBody t) Request m where
  type Attribute (JSONRequestBody t) Request = t
  type Absence (JSONRequestBody t) Request = Text

  derive :: Request -> m (Result (JSONRequestBody t) Request)
  derive r = do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ getRequestBodyChunk r
    pure $ case eitherDecode' (fromChunks chunks) of
             Left e  -> Refutation (pack e)
             Right t -> Proof r t

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
  probe @(JSONRequestBody t) >=> either (failHandler . mkError) (runKleisli handler)
  where
    mkError :: Text -> Response ByteString
    mkError e = Response
          { responseStatus  = badRequest400
          , responseHeaders = fromList []
          , responseBody    = Just $ fromStrict $ encodeUtf8 $ "Error parsing request body: " <> e
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
  pure $ link $ Response
    { responseStatus  = responseStatus x
    , responseHeaders = insert hContentType "application/json" $ responseHeaders x
    , responseBody    = encode <$> responseBody x
    }
