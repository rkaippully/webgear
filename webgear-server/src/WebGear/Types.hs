-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Common types and functions used throughout WebGear.
--
module WebGear.Types
  ( -- * WebGear Request
    -- | WebGear requests are WAI requests. This module reexports a number
    -- of useful functions that operate on requests from "Network.Wai"
    -- module.
    Request
  , remoteHost
  , httpVersion
  , isSecure
  , requestMethod
  , pathInfo
  , setPathInfo
  , queryString
  , requestHeaders
  , requestHeader
  , requestBodyLength
  , getRequestBodyChunk

    -- * WebGear Response
  , Response (..)
  , responseHeader
  , waiResponse

    -- * Creating responses
  , respond
  , continue100
  , switchingProtocols101
  , ok200
  , created201
  , accepted202
  , nonAuthoritative203
  , noContent204
  , resetContent205
  , partialContent206
  , multipleChoices300
  , movedPermanently301
  , found302
  , seeOther303
  , notModified304
  , temporaryRedirect307
  , permanentRedirect308
  , badRequest400
  , unauthorized401
  , paymentRequired402
  , forbidden403
  , notFound404
  , methodNotAllowed405
  , notAcceptable406
  , proxyAuthenticationRequired407
  , requestTimeout408
  , conflict409
  , gone410
  , lengthRequired411
  , preconditionFailed412
  , requestEntityTooLarge413
  , requestURITooLong414
  , unsupportedMediaType415
  , requestedRangeNotSatisfiable416
  , expectationFailed417
  , imATeapot418
  , unprocessableEntity422
  , preconditionRequired428
  , tooManyRequests429
  , requestHeaderFieldsTooLarge431
  , internalServerError500
  , notImplemented501
  , badGateway502
  , serviceUnavailable503
  , gatewayTimeout504
  , httpVersionNotSupported505
  , networkAuthenticationRequired511

  , Handler
  , Middleware
  , RequestMiddleware
  , ResponseMiddleware
  ) where

import Control.Arrow (Kleisli)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.Wai (Request, getRequestBodyChunk, httpVersion, isSecure, pathInfo, queryString,
                    remoteHost, requestBodyLength, requestHeaders, requestMethod)

import WebGear.Trait (Linked, link)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai


-- | Get the value of a request header
requestHeader :: HTTP.HeaderName -> Request -> Maybe ByteString
requestHeader h r = snd <$> find ((== h) . fst) (requestHeaders r)

-- | Get request with an updated URL path info.
setPathInfo :: [Text] -> Request -> Request
setPathInfo p r = r { pathInfo = p }

-- | A response sent from the server to the client.
--
-- The response contains a status, optional headers and an optional
-- body of type @a@.
data Response a = Response
    { responseStatus  :: HTTP.Status                            -- ^ Response status code
    , responseHeaders :: HM.HashMap HTTP.HeaderName ByteString  -- ^ Response headers
    , responseBody    :: Maybe a                                -- ^ Optional response body
    }

-- | Looks up a response header
responseHeader :: HTTP.HeaderName -> Response a -> Maybe ByteString
responseHeader h = HM.lookup h . responseHeaders

-- | Convert a WebGear response to a WAI Response.
waiResponse :: Response LBS.ByteString -> Wai.Response
waiResponse Response{..} = Wai.responseLBS
  responseStatus
  (HM.toList responseHeaders)
  (fromMaybe "" responseBody)

-- | A handler is a function from a request to response in a monadic
-- context. Both the request and the response can have linked traits.
--
-- The type level list @req@ contains all the traits expected to be
-- present in the request. The handler will produce a response that
-- satisfies all the traits in the type level list @res@.
type Handler m req res a = Kleisli m (Linked req Request) (Linked res (Response a))

-- | A middleware takes a handler as input and produces another
-- handler that usually adds some functionality.
--
-- A middleware can do a number of things with the request
-- handling such as:
--
--   * Change the request traits before invoking the handler.
--   * Change the response traits before passing it back to its caller.
--   * Use the linked value of any of the request or response traits.
--   * Change the response body.
--
type Middleware m req req' res' res a' a = Handler m req' res' a' -> Handler m req res a

-- | A middleware that manipulates only the request traits and leaves
-- the response unchanged.
type RequestMiddleware m req req' res a = Middleware m req req' res res a a

-- | A middleware that manipulates only the response traits and leaves
-- the request unchanged.
type ResponseMiddleware m req res' res a = Middleware m req req res' res a a


-- | Create a response with a given status and body
respond :: Monad m => HTTP.Status -> Maybe a -> m (Linked '[] (Response a))
respond s = pure . link . Response s mempty

-- | Continue 100 response
continue100 :: Monad m => m (Linked '[] (Response a))
continue100 = respond HTTP.continue100 Nothing

-- | Switching Protocols 101 response
switchingProtocols101 :: Monad m => m (Linked '[] (Response a))
switchingProtocols101 = respond HTTP.switchingProtocols101 Nothing

-- | OK 200 response
ok200 :: Monad m => a -> m (Linked '[] (Response a))
ok200 = respond HTTP.ok200 . Just

-- | Created 201 response
created201 :: Monad m => a -> m (Linked '[] (Response a))
created201 = respond HTTP.created201 . Just

-- | Accepted 202 response
accepted202 :: Monad m => a -> m (Linked '[] (Response a))
accepted202 = respond HTTP.accepted202 . Just

-- | Non-Authoritative 203 response
nonAuthoritative203 :: Monad m => a -> m (Linked '[] (Response a))
nonAuthoritative203 = respond HTTP.nonAuthoritative203 . Just

-- | No Content 204 response
noContent204 :: Monad m => m (Linked '[] (Response a))
noContent204 = respond HTTP.noContent204 Nothing

-- | Reset Content 205 response
resetContent205 :: Monad m => m (Linked '[] (Response a))
resetContent205 = respond HTTP.resetContent205 Nothing

-- | Partial Content 206 response
partialContent206 :: Monad m => a -> m (Linked '[] (Response a))
partialContent206 = respond HTTP.partialContent206 . Just

-- | Multiple Choices 300 response
multipleChoices300 :: Monad m => a -> m (Linked '[] (Response a))
multipleChoices300 = respond HTTP.multipleChoices300 . Just

-- | Moved Permanently 301 response
movedPermanently301 :: Monad m => a -> m (Linked '[] (Response a))
movedPermanently301 = respond HTTP.movedPermanently301 . Just

-- | Found 302 response
found302 :: Monad m => a -> m (Linked '[] (Response a))
found302 = respond HTTP.found302 . Just

-- | See Other 303 response
seeOther303 :: Monad m => a -> m (Linked '[] (Response a))
seeOther303 = respond HTTP.seeOther303 . Just

-- | Not Modified 304 response
notModified304 :: Monad m => m (Linked '[] (Response a))
notModified304 = respond HTTP.notModified304 Nothing

-- | Temporary Redirect 307 response
temporaryRedirect307 :: Monad m => a -> m (Linked '[] (Response a))
temporaryRedirect307 = respond HTTP.temporaryRedirect307 . Just

-- | Permanent Redirect 308 response
permanentRedirect308 :: Monad m => a -> m (Linked '[] (Response a))
permanentRedirect308 = respond HTTP.permanentRedirect308 . Just

-- | Bad Request 400 response
badRequest400 :: Monad m => a -> m (Linked '[] (Response a))
badRequest400 = respond HTTP.badRequest400 . Just

-- | Unauthorized 401 response
unauthorized401 :: Monad m => a -> m (Linked '[] (Response a))
unauthorized401 = respond HTTP.unauthorized401 . Just

-- | Payment Required 402 response
paymentRequired402 :: Monad m => a -> m (Linked '[] (Response a))
paymentRequired402 = respond HTTP.paymentRequired402 . Just

-- | Forbidden 403 response
forbidden403 :: Monad m => a -> m (Linked '[] (Response a))
forbidden403 = respond HTTP.forbidden403 . Just

-- | Not Found 404 response
notFound404 :: Monad m => m (Linked '[] (Response a))
notFound404 = respond HTTP.notFound404 Nothing

-- | Method Not Allowed 405 response
methodNotAllowed405 :: Monad m => a -> m (Linked '[] (Response a))
methodNotAllowed405 = respond HTTP.methodNotAllowed405 . Just

-- | Not Acceptable 406 response
notAcceptable406 :: Monad m => a -> m (Linked '[] (Response a))
notAcceptable406 = respond HTTP.notAcceptable406 . Just

-- | Proxy Authentication Required 407 response
proxyAuthenticationRequired407 :: Monad m => a -> m (Linked '[] (Response a))
proxyAuthenticationRequired407 = respond HTTP.proxyAuthenticationRequired407 . Just

-- | Request Timeout 408 response
requestTimeout408 :: Monad m => a -> m (Linked '[] (Response a))
requestTimeout408 = respond HTTP.requestTimeout408 . Just

-- | Conflict 409 response
conflict409 :: Monad m => a -> m (Linked '[] (Response a))
conflict409 = respond HTTP.conflict409 . Just

-- | Gone 410 response
gone410 :: Monad m => a -> m (Linked '[] (Response a))
gone410 = respond HTTP.gone410 . Just

-- | Length Required 411 response
lengthRequired411 :: Monad m => a -> m (Linked '[] (Response a))
lengthRequired411 = respond HTTP.lengthRequired411 . Just

-- | Precondition Failed 412 response
preconditionFailed412 :: Monad m => a -> m (Linked '[] (Response a))
preconditionFailed412 = respond HTTP.preconditionFailed412 . Just

-- | Request Entity Too Large 413 response
requestEntityTooLarge413 :: Monad m => a -> m (Linked '[] (Response a))
requestEntityTooLarge413 = respond HTTP.requestEntityTooLarge413 . Just

-- | Request URI Too Long 414 response
requestURITooLong414 :: Monad m => a -> m (Linked '[] (Response a))
requestURITooLong414 = respond HTTP.requestURITooLong414 . Just

-- | Unsupported Media Type 415 response
unsupportedMediaType415 :: Monad m => a -> m (Linked '[] (Response a))
unsupportedMediaType415 = respond HTTP.unsupportedMediaType415 . Just

-- | Requested Range Not Satisfiable 416 response
requestedRangeNotSatisfiable416 :: Monad m => a -> m (Linked '[] (Response a))
requestedRangeNotSatisfiable416 = respond HTTP.requestedRangeNotSatisfiable416 . Just

-- | Expectation Failed 417 response
expectationFailed417 :: Monad m => a -> m (Linked '[] (Response a))
expectationFailed417 = respond HTTP.expectationFailed417 . Just

-- | I'm A Teapot 418 response
imATeapot418 :: Monad m => a -> m (Linked '[] (Response a))
imATeapot418 = respond HTTP.imATeapot418 . Just

-- | Unprocessable Entity 422 response
unprocessableEntity422 :: Monad m => a -> m (Linked '[] (Response a))
unprocessableEntity422 = respond HTTP.unprocessableEntity422 . Just

-- | Precondition Required 428 response
preconditionRequired428 :: Monad m => a -> m (Linked '[] (Response a))
preconditionRequired428 = respond HTTP.preconditionRequired428 . Just

-- | Too Many Requests 429 response
tooManyRequests429 :: Monad m => a -> m (Linked '[] (Response a))
tooManyRequests429 = respond HTTP.tooManyRequests429 . Just

-- | Request Header Fields Too Large 431 response
requestHeaderFieldsTooLarge431 :: Monad m => a -> m (Linked '[] (Response a))
requestHeaderFieldsTooLarge431 = respond HTTP.requestHeaderFieldsTooLarge431 . Just

-- | Internal Server Error 500 response
internalServerError500 :: Monad m => a -> m (Linked '[] (Response a))
internalServerError500 = respond HTTP.internalServerError500 . Just

-- | Not Implemented 501 response
notImplemented501 :: Monad m => a -> m (Linked '[] (Response a))
notImplemented501 = respond HTTP.notImplemented501 . Just

-- | Bad Gateway 502 response
badGateway502 :: Monad m => a -> m (Linked '[] (Response a))
badGateway502 = respond HTTP.badGateway502 . Just

-- | Service Unavailable 503 response
serviceUnavailable503 :: Monad m => a -> m (Linked '[] (Response a))
serviceUnavailable503 = respond HTTP.serviceUnavailable503 . Just

-- | Gateway Timeout 504 response
gatewayTimeout504 :: Monad m => a -> m (Linked '[] (Response a))
gatewayTimeout504 = respond HTTP.gatewayTimeout504 . Just

-- | HTTP Version Not Supported 505 response
httpVersionNotSupported505 :: Monad m => a -> m (Linked '[] (Response a))
httpVersionNotSupported505 = respond HTTP.httpVersionNotSupported505 . Just

-- | Network Authentication Required 511 response
networkAuthenticationRequired511 :: Monad m => a -> m (Linked '[] (Response a))
networkAuthenticationRequired511 = respond HTTP.networkAuthenticationRequired511 . Just
