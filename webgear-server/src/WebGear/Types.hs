{-# OPTIONS_GHC -ddump-splices #-}
{-|
Description      : Common types used in WebGear
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com
-}
module WebGear.Types
  ( -- * HTTP Request and Response
    Request (..)
  , requestMethod
  , requestHeader
  , requestPath
  , setRequestPath
  , requestBodyNextChunk
  , Response (..)
  , waiResponse
  , addResponseHeader

  , MonadRouter

  , Handler
  , Middleware
  , RequestMiddleware
  , ResponseMiddleware

  , ok
  , noContent
  , badRequest
  , notFound
  ) where

import Control.Arrow
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import WebGear.Trait


-- | An HTTP request sent to the server
newtype Request = Request { waiRequest :: Wai.Request }

-- | The HTTP method of the request
requestMethod :: Request -> HTTP.Method
requestMethod = Wai.requestMethod . waiRequest

-- | Get the value of a request header
requestHeader :: HTTP.HeaderName -> Request -> Maybe ByteString
requestHeader h r = snd <$> find ((== h) . fst) (Wai.requestHeaders $ waiRequest r)

requestBodyNextChunk :: Request -> IO ByteString
requestBodyNextChunk = Wai.getRequestBodyChunk . waiRequest

requestPath :: Request -> [Text]
requestPath = Wai.pathInfo . waiRequest

setRequestPath :: [Text] -> Request -> Request
setRequestPath p r = r { waiRequest = (waiRequest r) { Wai.pathInfo = p } }

-- | An HTTP response sent from the server
data Response a = Response
    { respStatus  :: HTTP.Status
    , respHeaders :: HashMap HTTP.HeaderName ByteString
    , respBody    :: Maybe a
    }

waiResponse :: Response LByteString -> Wai.Response
waiResponse Response{..} = Wai.responseLBS respStatus (HM.toList respHeaders) (fromMaybe "" respBody)

addResponseHeader :: HTTP.Header -> Response a -> Response a
addResponseHeader (name, val) resp = resp { respHeaders = HM.insertWith f name val (respHeaders resp) }
  where
    f = flip const


type Handler m (req :: [Type]) (res :: [Type]) a = Kleisli m (Linked req Request) (Linked res (Response a))

type Middleware m req req' res' res a' a = Handler m req' res' a' -> Handler m req res a

type RequestMiddleware m req req' res a = Middleware m req req' res res a a

type ResponseMiddleware m req res' res a = Middleware m req req res' res a a


type MonadRouter m = (Alternative m, MonadPlus m, MonadError (Maybe (First Wai.Response)) m)


-- | Respond with a 200 OK
ok :: Monad m => a -> m (Linked '[] (Response a))
ok = pure . linkzero . Response HTTP.ok200 HM.empty . Just

-- | Respond with a 400 Bad Request
badRequest :: Monad m => m (Linked '[] (Response a))
badRequest = pure $ linkzero $ Response HTTP.badRequest400 HM.empty Nothing

-- | Respond with a 404 NotFound
notFound :: Monad m => m (Linked '[] (Response a))
notFound = pure $ linkzero $ Response HTTP.notFound404 HM.empty Nothing

noContent :: (Monad m, IsString s) => m (Linked '[] (Response s))
noContent = pure $ linkzero $ Response HTTP.noContent204 HM.empty $ Just ""
