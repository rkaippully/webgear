-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Common types and functions used throughout WebGear.
--
module WebGear.Types
  ( -- * WebGear Request
    Request
  , remoteHost
  , httpVersion
  , isSecure
  , requestMethod
  , pathInfo
  , queryString
  , requestHeader
  , requestHeaders
  , requestBodyLength
  , getRequestBodyChunk

    -- * WebGear Response
  , Response (..)
  , responseHeader
  , setResponseHeader
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

    -- * Handlers and Middlewares
  , Handler'
  , Handler
  , Middleware'
  , Middleware
  , RequestMiddleware'
  , RequestMiddleware
  , ResponseMiddleware'
  , ResponseMiddleware

    -- * Routing
  , Router (..)
  , MonadRouter (..)
  , PathInfo (..)
  , RouteError (..)
  , transform
  , runRoute
  , toApplication

    -- * Modifiers
  , Existence (..)
  , ParseStyle (..)
  ) where

import Control.Applicative (Alternative)
import Control.Arrow (Kleisli (..))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad (MonadPlus)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion.To (ToByteString, toByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup (..), stimesIdempotent)
import Data.String (fromString)
import Data.Text (Text)
import Data.Version (showVersion)
import GHC.Exts (fromList)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, getRequestBodyChunk, httpVersion, isSecure, pathInfo, queryString,
                    remoteHost, requestBodyLength, requestHeaders, requestMethod)
import qualified Network.Wai as Wai
import Paths_webgear_server (version)
import WebGear.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Trait (Linked, linkzero)


-- | Get the value of a request header
requestHeader :: HTTP.HeaderName -> Request -> Maybe ByteString
requestHeader h r = snd <$> find ((== h) . fst) (requestHeaders r)

-- | An HTTP response sent from the server to the client.
--
-- The response contains a status, optional headers and an optional
-- body of type @a@.
data Response a = Response
    { responseStatus  :: HTTP.Status                            -- ^ Response status code
    , responseHeaders :: HM.HashMap HTTP.HeaderName ByteString  -- ^ Response headers
    , responseBody    :: Maybe a                                -- ^ Optional response body
    }
    deriving stock (Eq, Ord, Show, Functor)

-- | Looks up a response header
responseHeader :: HTTP.HeaderName -> Response a -> Maybe ByteString
responseHeader h = HM.lookup h . responseHeaders

-- | Set a response header value
setResponseHeader :: HTTP.HeaderName -> ByteString -> Response a -> Response a
setResponseHeader name val r = r { responseHeaders = HM.insert name val (responseHeaders r) }

-- | Convert a WebGear response to a WAI Response.
waiResponse :: Response LBS.ByteString -> Wai.Response
waiResponse Response{..} = Wai.responseLBS
  responseStatus
  (HM.toList responseHeaders)
  (fromMaybe "" responseBody)


-- | Create a response with a given status and body
respond :: HTTP.Status -> Maybe a -> Response a
respond s = Response s mempty

-- | Continue 100 response
continue100 :: Response a
continue100 = respond HTTP.continue100 Nothing

-- | Switching Protocols 101 response
switchingProtocols101 :: Response a
switchingProtocols101 = respond HTTP.switchingProtocols101 Nothing

-- | OK 200 response
ok200 :: a -> Response a
ok200 = respond HTTP.ok200 . Just

-- | Created 201 response
created201 :: a -> Response a
created201 = respond HTTP.created201 . Just

-- | Accepted 202 response
accepted202 :: a -> Response a
accepted202 = respond HTTP.accepted202 . Just

-- | Non-Authoritative 203 response
nonAuthoritative203 :: a -> Response a
nonAuthoritative203 = respond HTTP.nonAuthoritative203 . Just

-- | No Content 204 response
noContent204 :: Response a
noContent204 = respond HTTP.noContent204 Nothing

-- | Reset Content 205 response
resetContent205 :: Response a
resetContent205 = respond HTTP.resetContent205 Nothing

-- | Partial Content 206 response
partialContent206 :: a -> Response a
partialContent206 = respond HTTP.partialContent206 . Just

-- | Multiple Choices 300 response
multipleChoices300 :: a -> Response a
multipleChoices300 = respond HTTP.multipleChoices300 . Just

-- | Moved Permanently 301 response
movedPermanently301 :: a -> Response a
movedPermanently301 = respond HTTP.movedPermanently301 . Just

-- | Found 302 response
found302 :: a -> Response a
found302 = respond HTTP.found302 . Just

-- | See Other 303 response
seeOther303 :: a -> Response a
seeOther303 = respond HTTP.seeOther303 . Just

-- | Not Modified 304 response
notModified304 :: Response a
notModified304 = respond HTTP.notModified304 Nothing

-- | Temporary Redirect 307 response
temporaryRedirect307 :: a -> Response a
temporaryRedirect307 = respond HTTP.temporaryRedirect307 . Just

-- | Permanent Redirect 308 response
permanentRedirect308 :: a -> Response a
permanentRedirect308 = respond HTTP.permanentRedirect308 . Just

-- | Bad Request 400 response
badRequest400 :: a -> Response a
badRequest400 = respond HTTP.badRequest400 . Just

-- | Unauthorized 401 response
unauthorized401 :: a -> Response a
unauthorized401 = respond HTTP.unauthorized401 . Just

-- | Payment Required 402 response
paymentRequired402 :: a -> Response a
paymentRequired402 = respond HTTP.paymentRequired402 . Just

-- | Forbidden 403 response
forbidden403 :: a -> Response a
forbidden403 = respond HTTP.forbidden403 . Just

-- | Not Found 404 response
notFound404 :: Response a
notFound404 = respond HTTP.notFound404 Nothing

-- | Method Not Allowed 405 response
methodNotAllowed405 :: a -> Response a
methodNotAllowed405 = respond HTTP.methodNotAllowed405 . Just

-- | Not Acceptable 406 response
notAcceptable406 :: a -> Response a
notAcceptable406 = respond HTTP.notAcceptable406 . Just

-- | Proxy Authentication Required 407 response
proxyAuthenticationRequired407 :: a -> Response a
proxyAuthenticationRequired407 = respond HTTP.proxyAuthenticationRequired407 . Just

-- | Request Timeout 408 response
requestTimeout408 :: a -> Response a
requestTimeout408 = respond HTTP.requestTimeout408 . Just

-- | Conflict 409 response
conflict409 :: a -> Response a
conflict409 = respond HTTP.conflict409 . Just

-- | Gone 410 response
gone410 :: a -> Response a
gone410 = respond HTTP.gone410 . Just

-- | Length Required 411 response
lengthRequired411 :: a -> Response a
lengthRequired411 = respond HTTP.lengthRequired411 . Just

-- | Precondition Failed 412 response
preconditionFailed412 :: a -> Response a
preconditionFailed412 = respond HTTP.preconditionFailed412 . Just

-- | Request Entity Too Large 413 response
requestEntityTooLarge413 :: a -> Response a
requestEntityTooLarge413 = respond HTTP.requestEntityTooLarge413 . Just

-- | Request URI Too Long 414 response
requestURITooLong414 :: a -> Response a
requestURITooLong414 = respond HTTP.requestURITooLong414 . Just

-- | Unsupported Media Type 415 response
unsupportedMediaType415 :: a -> Response a
unsupportedMediaType415 = respond HTTP.unsupportedMediaType415 . Just

-- | Requested Range Not Satisfiable 416 response
requestedRangeNotSatisfiable416 :: a -> Response a
requestedRangeNotSatisfiable416 = respond HTTP.requestedRangeNotSatisfiable416 . Just

-- | Expectation Failed 417 response
expectationFailed417 :: a -> Response a
expectationFailed417 = respond HTTP.expectationFailed417 . Just

-- | I'm A Teapot 418 response
imATeapot418 :: a -> Response a
imATeapot418 = respond HTTP.imATeapot418 . Just

-- | Unprocessable Entity 422 response
unprocessableEntity422 :: a -> Response a
unprocessableEntity422 = respond HTTP.unprocessableEntity422 . Just

-- | Precondition Required 428 response
preconditionRequired428 :: a -> Response a
preconditionRequired428 = respond HTTP.preconditionRequired428 . Just

-- | Too Many Requests 429 response
tooManyRequests429 :: a -> Response a
tooManyRequests429 = respond HTTP.tooManyRequests429 . Just

-- | Request Header Fields Too Large 431 response
requestHeaderFieldsTooLarge431 :: a -> Response a
requestHeaderFieldsTooLarge431 = respond HTTP.requestHeaderFieldsTooLarge431 . Just

-- | Internal Server Error 500 response
internalServerError500 :: a -> Response a
internalServerError500 = respond HTTP.internalServerError500 . Just

-- | Not Implemented 501 response
notImplemented501 :: a -> Response a
notImplemented501 = respond HTTP.notImplemented501 . Just

-- | Bad Gateway 502 response
badGateway502 :: a -> Response a
badGateway502 = respond HTTP.badGateway502 . Just

-- | Service Unavailable 503 response
serviceUnavailable503 :: a -> Response a
serviceUnavailable503 = respond HTTP.serviceUnavailable503 . Just

-- | Gateway Timeout 504 response
gatewayTimeout504 :: a -> Response a
gatewayTimeout504 = respond HTTP.gatewayTimeout504 . Just

-- | HTTP Version Not Supported 505 response
httpVersionNotSupported505 :: a -> Response a
httpVersionNotSupported505 = respond HTTP.httpVersionNotSupported505 . Just

-- | Network Authentication Required 511 response
networkAuthenticationRequired511 :: a -> Response a
networkAuthenticationRequired511 = respond HTTP.networkAuthenticationRequired511 . Just



-- | A handler is a function from a request to response in a monadic
-- context. Both the request and the response can have linked traits.
--
-- The type level list @req@ contains all the traits expected to be
-- present in the request.
type Handler' m req a = Kleisli m (Linked req Request) (Response a)

-- | A handler that runs on the 'Router' monad.
type Handler req a = Handler' Router req a

-- | A middleware takes a handler as input and produces another
-- handler that usually adds some functionality.
--
-- A middleware can do a number of things with the request
-- handling such as:
--
--   * Change the request traits before invoking the handler.
--   * Use the linked value of any of the request traits.
--   * Change the response body.
--
type Middleware' m req req' a' a = Handler' m req' a' -> Handler' m req a

-- | A middleware that runs on the 'Router' monad.
type Middleware req req' a' a = Middleware' Router req req' a' a

-- | A middleware that manipulates only the request traits and passes
-- the response through.
type RequestMiddleware' m req req' a = Middleware' m req req' a a

-- | A request middleware that runs on the 'Router' monad.
type RequestMiddleware req req' a = RequestMiddleware' Router req req' a

-- | A middleware that manipulates only the response and passes the
-- request through.
type ResponseMiddleware' m req a' a = Middleware' m req req a' a

-- | A response middleware that runs on the 'Router' monad.
type ResponseMiddleware req a' a = ResponseMiddleware' Router req a' a

-- | A natural transformation of handler monads.
--
-- This is useful if you want to run a handler in a monad other than
-- 'Router'.
--
transform :: (forall x. m x -> n x) -> Handler' m req a -> Handler' n req a
transform f (Kleisli mf) = Kleisli $ f . mf

-- | The path components to be matched by routing machinery
newtype PathInfo = PathInfo [Text]

-- | Responses that cause routes to abort execution
data RouteError = RouteMismatch
                  -- ^ A route did not match and the next one can be
                  -- tried
                | ErrorResponse (Response LBS.ByteString)
                  -- ^ A route matched but returned a short circuiting
                  -- error response
                deriving (Eq, Ord, Show)

instance Semigroup RouteError where
  RouteMismatch <> e = e
  e <> _             = e

  stimes :: Integral b => b -> RouteError -> RouteError
  stimes = stimesIdempotent

instance Monoid RouteError where
  mempty = RouteMismatch

-- | The monad for routing.
newtype Router a = Router
  { unRouter :: StateT PathInfo (ExceptT RouteError IO) a }
  deriving newtype ( Functor, Applicative, Alternative, Monad, MonadPlus
                   , MonadError RouteError
                   , MonadState PathInfo
                   , MonadIO
                   , MonadThrow, MonadCatch
                   )

-- | HTTP request routing with short circuiting behavior.
class (MonadState PathInfo m, Alternative m, MonadPlus m) => MonadRouter m where
  -- | Mark the current route as rejected, alternatives can be tried
  rejectRoute :: m a

  -- | Short-circuit the current handler and return a response
  errorResponse :: Response LBS.ByteString -> m a

  -- | Handle an error response
  catchErrorResponse :: m a -> (Response LBS.ByteString -> m a) -> m a

instance MonadRouter Router where
  rejectRoute :: Router a
  rejectRoute = throwError RouteMismatch

  errorResponse :: Response LBS.ByteString -> Router a
  errorResponse = throwError . ErrorResponse

  catchErrorResponse :: Router a -> (Response LBS.ByteString -> Router a) -> Router a
  catchErrorResponse action handle = action `catchError` f
    where
      f RouteMismatch       = rejectRoute
      f (ErrorResponse res) = handle res


-- | Convert a routable handler into a plain function from request to response.
runRoute :: ToByteString a => Handler '[] a -> (Wai.Request -> IO Wai.Response)
runRoute route req = waiResponse . addServerHeader . either routeErrorToResponse id <$> runRouter
  where
    runRouter :: IO (Either RouteError (Response LBS.ByteString))
    runRouter = fmap (fmap (fmap toByteString))
                $ runExceptT
                $ flip evalStateT (PathInfo $ pathInfo req)
                $ unRouter
                $ runKleisli route
                $ linkzero req

    routeErrorToResponse :: RouteError -> Response LBS.ByteString
    routeErrorToResponse RouteMismatch     = notFound404
    routeErrorToResponse (ErrorResponse r) = r

    addServerHeader :: Response LBS.ByteString -> Response LBS.ByteString
    addServerHeader r = r { responseHeaders = responseHeaders r <> fromList [serverHeader] }

    serverHeader :: HTTP.Header
    serverHeader = (HTTP.hServer, fromString $ "WebGear/" ++ showVersion version)

-- | Convert a routable handler into a Wai application
toApplication :: ToByteString a => Handler '[] a -> Wai.Application
toApplication route request next = runRoute route request >>= next
