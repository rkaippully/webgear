{-|
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com

This module contains some types and functions used throughout WebGear.
-}
module WebGear.Types
  ( -- * WebGear Request
    {- | WebGear requests are WAI requests. This module reexports a number
      of useful functions that operate on requests from "Network.Wai"
      module.
    -}
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
  , waiResponse
  , addResponseHeader

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
import Network.HTTP.Types (Header, HeaderName, Status)
import Network.Wai (Request, getRequestBodyChunk, httpVersion, isSecure, pathInfo, queryString,
                    remoteHost, requestBodyLength, requestHeaders, requestMethod)

import WebGear.Trait (Linked)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Network.Wai as Wai


-- | Get the value of a request header
requestHeader :: HeaderName -> Request -> Maybe ByteString
requestHeader h r = snd <$> find ((== h) . fst) (requestHeaders r)

-- | Get request with an updated URL path info.
setPathInfo :: [Text] -> Request -> Request
setPathInfo p r = r { pathInfo = p }

{- | A response sent from the server to the client.

The response contains a status, optional headers and an optional body
of type @a@.
-}
data Response a = Response
    { respStatus  :: Status                            -- ^ Response status code
    , respHeaders :: HM.HashMap HeaderName ByteString  -- ^ Response headers
    , respBody    :: Maybe a                           -- ^ Optional response body
    }

-- | Convert a WebGear response to a WAI Response.
waiResponse :: Response LBS.ByteString -> Wai.Response
waiResponse Response{..} = Wai.responseLBS respStatus (HM.toList respHeaders) (fromMaybe "" respBody)

-- | Create or update a response header.
addResponseHeader :: Header -> Response a -> Response a
addResponseHeader (name, val) resp = resp { respHeaders = HM.insertWith f name val (respHeaders resp) }
  where
    f = flip const

{- | A handler is a function from a request to response in a monadic
   context. Both the request and the response can have linked traits.

   The type level list @req@ contains all the traits expected to be
   present in the request. The handler will produce a response that
   satisfies all the traits in the type level list @res@.
-}
type Handler m req res a = Kleisli m (Linked req Request) (Linked res (Response a))

{- | A middleware takes a handler as input and produces another handler
   that usually adds some functionality.

   The middleware can do a number of things with the request
   handling. It can change the request traits before invoking the
   handler. It can also change the response traits before passing it
   back to its caller. It can make use of the linked value of any of
   the request or response traits. It can also change the response
   body.
-}
type Middleware m req req' res' res a' a = Handler m req' res' a' -> Handler m req res a

{- | A middleware that manipulates only the request traits and leaves
   the response unchanged.
-}
type RequestMiddleware m req req' res a = Middleware m req req' res res a a

{- | A middleware that manipulates only the response traits and leaves
   the request unchanged.
-}
type ResponseMiddleware m req res' res a = Middleware m req req res' res a a
