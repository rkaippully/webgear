{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- WebGear helps to build composable, type-safe HTTP API servers.
--
-- The documentation below gives an overview of WebGear. Example
-- programs built using WebGear are available at
-- https://github.com/rkaippully/webgear/tree/master/webgear-examples.
--
module WebGear
  ( -- * Serving HTTP APIs
    -- $serving

    -- * Traits and Linking
    -- $traits

    -- * Handlers
    -- $handlers

    -- * Middlewares
    -- $middlewares

    -- * Routing
    -- $routing

    -- * Running the Server
    -- $running

    -- * Servers with other monads
    -- $otherMonads

    module Control.Applicative
  , module Control.Arrow
  , module Data.ByteString.Lazy
  , module Data.ByteString.Conversion.To
  , module Data.Tagged
  , module Data.Text
  , module Web.HttpApiData
  , module WebGear.Middlewares
  , module WebGear.Trait
  , module WebGear.Types
  ) where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (Kleisli (..))
import Data.ByteString.Conversion.To
import Data.ByteString.Lazy (ByteString)
import Data.Tagged
import Data.Text
import Web.HttpApiData (FromHttpApiData)

import qualified Network.Wai as Wai

import WebGear.Middlewares
import WebGear.Trait
import WebGear.Types


--
-- $serving
--
-- An HTTP API server handler can be thought of as a function that
-- takes a request as input and produces a response as output in a
-- monadic context.
--
-- > handler :: Monad m => Request -> m Response
--
-- For reasons that will be explained later, WebGear uses the 'Router'
-- monad for running handlers. Thus the above type signature changes
-- to:
--
-- > handler :: Request -> Router Response
--
-- Most APIs will require extracting some information from the
-- request, processing it and then producing a response. For example,
-- the server might require access to some HTTP header values, query
-- parameters, or the request body. WebGear allows to access such
-- information using traits.
--
--
-- $traits
--
-- A trait is an attribute associated with a value. For example, a
-- @Request@ might have a header that we are interested in, which is
-- represented by the 'Header' trait. All traits have instances of the
-- 'Trait' typeclass. The 'toAttribute' function helps to check
-- presence of the trait. It also has two associated types -
-- 'Attribute' and 'Absence' - to represent the result of the
-- extraction.
--
-- For example, the 'Header' trait has an instance of the 'Trait'
-- typeclass. The 'toAttribute' function evaluates to a 'Proof' or
-- 'Refutation' value depending on whether we can successfully
-- retrieve the header value.
--
-- WebGear provides type-safety by linking traits to the request at
-- type level. The 'Linked' data type associates a 'Request' with a
-- list of traits. This linking guarantees that the Request has the
-- specified trait.
--
-- These functions work with traits and linked values:
--
--   * 'link': Establish a link between a value and an empty list of
--     traits. This always succeeds.
--
--   * 'unlink': Convert a linked value to a regular value without any
--     type-level traits.
--
--   * 'probe': Attempts to establish a link between a linked value
--     with an additional trait using 'toAttribute'.
--
--   * 'remove': Removes a trait from the list of linked traits.
--
--   * 'get': Extract an 'Attribute' associated with a trait from a
--     linked value.
--
-- For example, we make use of the @'Method' \@GET@ trait to ensure
-- that our handler is called only for GET requests. We can link a
-- request value with this trait using:
--
-- @
-- linkedRequest :: Monad m => 'Request' -> 'Router' (Either 'MethodMismatch' ('Linked' '['Method' GET] 'Request'))
-- linkedRequest = 'probe' @('Method' GET) . 'link'
-- @
--
-- Let us modify the type signature of our handler to use linked
-- values instead of regular values:
--
-- > handler :: Linked req Request -> Router Response
--
-- Here, @req@ is a type-level list of traits associated with the
-- @Request@ that this handler requires. This ensures that this
-- handler can only be called with a request possessing certain
-- traits thus providing type-safety to our handlers.
--
--
-- $handlers
--
-- Handlers in WebGear are defined with a type very similar to the
-- above.
--
-- @
-- type 'Handler'' m req a = 'Kleisli' m ('Linked' req 'Request') ('Response' a)
--
-- type 'Handler' req a = 'Handler'' 'Router' req a
-- @
--
-- It is a 'Kleisli' arrow as described in the above section with
-- type-level trait lists. However, the response is parameterized by
-- the type variable @a@, which represents the type of the response
-- body.
--
-- 'Handler'' can work with any monad while 'Handler' works with
-- 'Router'.
--
-- A handler can extract some trait attribute of a request with the
-- 'get' function.
--
--
-- $middlewares
--
-- A middleware is a higher-order function that takes a handler as
-- input and produces another handler with potentially different
-- request and response types. Thus middlewares can augment the
-- functionality of another handler.
--
-- For example, here is the definition of the 'method' middleware:
--
-- @
-- method :: ('IsStdMethod' t, 'MonadRouter' m) => 'Handler'' m ('Method' t:req) a -> 'Handler'' m req a
-- method handler = 'Kleisli' $ 'probe' \@('Method' t) >=> 'either' ('const' 'rejectRoute') ('runKleisli' handler)
-- @
--
-- The @probe \@(Method t)@ function is used to ensure that the
-- request has method @t@ before invoking the @handler@. In case of a
-- mismatch, this route is rejected by calling 'rejectRoute'.
--
-- Many middlewares can be composed to form complex request handling
-- logic. For example:
--
-- @
-- putUser = 'method' \@PUT
--           $ 'requestContentTypeHeader' \@"application/json"
--           $ 'jsonRequestBody' \@User
--           $ 'jsonResponseBody' \@User
--           $ putUserHandler
-- @
--
--
-- $routing
--
-- A typical server will have many routes and we would like to pick
-- one based on the URL path, HTTP method etc. We need a couple of
-- things to achieve this.
--
-- First, we need a way to indicate that a handler cannot handle a
-- request, possibly because the path or method did not match with
-- what was expected. This is achieved by the 'rejectRoute' function:
--
-- @
-- class (Alternative m, MonadPlus m) => 'MonadRouter' m where
--   'rejectRoute' :: m a
--   'errorResponse' :: 'Response' 'ByteString' -> m a
--   'catchErrorResponse' :: m a -> ('Response' 'ByteString' -> m a) -> m a
-- @
--
-- The 'errorResponse' can be used in cases where we find a matching
-- route but the request handling is aborted for some reason. For
-- example, if a route requires the request Content-type header to
-- have a particular value but the actual request had a different
-- Content-type, 'errorResponse' can be used to abort and return an
-- error response.
--
-- Second, we need a mechanism to try an alternate route when one
-- route is rejected. Since 'MonadRouter' is an 'Alternative', we can
-- use '<|>' to combine many routes. When a request arrives, a match
-- will be attempted against each route sequentially and the first
-- matching route handler will process the request. Here is an
-- example:
--
-- @
-- allRoutes :: 'Handler' '[] 'ByteString'
-- allRoutes = ['match'| /v1\/users\/userId:Int |]    -- non-TH version: 'path' \@"/v1/users" . 'pathVar' \@"userId" \@Int
--             $ getUser \<|\> putUser \<|\> deleteUser
--
-- type IntUserId = 'PathVar' "userId" Int
--
-- getUser :: 'Has' IntUserId req => 'Handler' req 'ByteString'
-- getUser = 'method' \@GET getUserHandler
--
-- putUser :: 'Has' IntUserId req => 'Handler' req 'ByteString'
-- putUser = 'method' \@PUT
--           $ 'requestContentTypeHeader' \@"application/json"
--           $ 'jsonRequestBody' \@User
--           $ putUserHandler
--
-- deleteUser :: 'Has' IntUserId req => 'Handler' req 'ByteString'
-- deleteUser = 'method' \@DELETE deleteUserHandler
-- @
--
--
-- $running
--
-- Routable handlers can be converted to a Wai 'Wai.Application' using
-- 'toApplication':
--
-- @
-- toApplication :: 'ToByteString' a => 'Handler' '[] a -> 'Wai.Application'
-- @
--
-- This Wai application can then be run as a Warp web server.
--
-- @
-- main :: IO ()
-- main = Warp.run 3000 $ 'toApplication' allRoutes
-- @
--
--
-- $otherMonads
--
-- It may not be practical to use 'Router' monad for your handlers. In
-- most cases, you would need your own monad transformer stack or
-- algebraic effect runners. WebGear supports that easily.
--
-- Let us say, the @putUserHandler@ from the above example runs on
-- some monad other than 'Router'. You can still use it as a handler thus:
--
-- @
-- putUser = 'method' \@PUT
--           $ 'requestContentTypeHeader' \@"application/json"
--           $ 'jsonRequestBody' \@User
--           $ 'jsonResponseBody' \@User
--           $ 'transform' customMonadToRouter putUserHandler
--
-- putUserHandler :: 'Handler'' MyCustomMonad req User
-- putUserHandler = ....
--
-- customMonadToRouter :: MyCustomMonad a -> Router a
-- customMonadToRouter = ...
-- @
--
-- As long as you have a way of transforming values in your custom
-- monad to a 'Router' monadic value, you can use 'transform' to
-- convert the handlers in that custom monad to handlers running in
-- 'Router' monad.
--
