{-# OPTIONS_GHC -Wno-unused-imports #-}
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
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow (Kleisli (..))
import Web.HttpApiData (FromHttpApiData)

import qualified Network.Wai as Wai

import WebGear.Middlewares
import WebGear.Route
import WebGear.Trait
import WebGear.Trait.Body
import WebGear.Trait.Header
import WebGear.Trait.Method
import WebGear.Trait.Path
import WebGear.Types


--
-- $serving
--
-- An HTTP API server handler can be thought of as a function that
-- takes a request as input and produces a response as output in a
-- monadic context.
--
-- @
-- handler :: Monad m => 'Request' -> m 'Response'
-- @
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
-- @Request@ might have a header that we are interested in; the
-- 'Header' trait represents that. All traits have instances of
-- 'Trait' typeclass. This typeclass helps to 'check' the presence of
-- the trait. It also has two associated types - 'Val' and 'Fail' - to
-- represent the result of 'check'ing the presence of a trait.
--
-- For example, the 'Header' trait has an instance of the 'Trait'
-- typeclass. The 'check' function evaluates to a 'CheckSuccess' value
-- if the header exists and can be converted to an attribute via the
-- 'FromHttpApiData' typeclass. Otherwise, it evaluates to a
-- 'CheckFail' value.
--
-- WebGear provides type-safety by linking traits to the request or
-- response at type level. The 'Linked' data type associates a
-- 'Request' or 'Response' with a list of traits. This linking
-- guarantees that the Request or Response has the specified trait.
--
-- These functions work with traits and linked values:
--
--   * 'linkzero': Establish a link between a value and an empty list
--     of traits. This always succeeds.
--   * 'linkplus': Attempts to establish a link between a linked value
--     with an additional trait.
--   * 'linkminus': Removes a trait from the list of linked traits.
--   * 'unlink': Convert a linked value to a regular value without any
--     type-level traits.
--   * 'traitValue': Extract a 'Val' associated with a trait from a
--     linked value.
--
-- For example, we make use of the @'Method' \@GET@ trait to ensure
-- that our handler is called only for GET requests. We can link a
-- request value with this trait using:
--
-- @
-- linkedRequest :: Monad m => 'Request' -> m (Either 'MethodMismatch' ('Linked' '['Method' GET] 'Request'))
-- linkedRequest = 'linkplus' @('Method' GET) . 'linkzero'
-- @
--
-- Let us modify the type signature of our handler to use linked
-- values instead of regular values:
--
-- > handler :: Monad m => Linked req Request -> m (Linked res Response)
--
-- Here, @req@ is a type-level list of traits associated with the
-- @Request@ that this handler requires and @res@ is a type-level list
-- of traits associated with the @Response@ that this handler will
-- produce. This implies that this handler can be called only with a
-- request possessing certain traits and it is guaranteed to produce a
-- response having certain traits.
--
--
-- $handlers
--
-- Handlers in WebGear are defined with a type very similar to the
-- above.
--
-- @
-- type 'Handler' m req res a = 'Kleisli' m ('Linked' req 'Request') ('Linked' res ('Response' a))
-- @
--
-- It is a 'Kleisli' arrow as described in the above section with
-- type-level trait lists. However, the response is parameterized by
-- the type variable @a@, which represents the type of the response
-- body.
--
-- A handler can extract some trait attribute of a request with the
-- 'traitValue' function. It can also use 'linkplus' function to prove
-- the presence of traits in the response before returning it.
--
--
-- $middlewares
--
-- A middleware is a higher-order function that takes a handler as
-- input and produces another handler with potentially different lists
-- of request and response traits. Thus middlewares can augment the
-- functionality of another handler.
--
-- For example, here is the definition of the 'method' middleware:
--
-- @
-- method :: ('IsStdMethod' t, 'MonadRouter' m) => 'Handler' m ('Method' t:req) res a -> 'Handler' m req res a
-- method handler = 'Kleisli' $ 'linkplus' \@('Method' t) >=> 'either' ('const' 'rejectRoute') ('runKleisli' handler)
-- @
--
-- The @linkplus \@(Method t)@ function is used to prove the presence
-- of the method @t@ in the request and the @handler@ is invoked only
-- if the method matches. In case of a mismatch, this route is
-- rejected by calling 'rejectRoute'.
--
-- Many middlewares can be composed to form complex request handling
-- logic.
--
-- @
-- putUser = 'method' \@PUT
--           $ 'requestContentType' \@"application/json"
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
--   'failHandler' :: 'Response' ByteString -> m a
-- @
--
-- The 'failHandler' can be used in cases where we find a matching
-- route but the request handling is aborted for some reason. For
-- example, if a route requires the request Content-type header to
-- have a particular value but the actual request had a different
-- Content-type, 'failHandler' can be used to abort and return an
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
-- allRoutes :: 'MonadRouter' m => 'Handler' m '[] '[] ByteString
-- allRoutes = ['match'| v1\/users\/userId:Int |]    -- non-TH version: 'path' \@"v1/users" . 'pathVar' \@"userId" \@Int
--             $ getUser \<|\> putUser \<|\> deleteUser
--
-- type IntUserId = 'PathVar' "userId" Int
--
-- getUser :: ('MonadRouter' m, 'Has' IntUserId req) => 'Handler' m req '[] ByteString
-- getUser = 'method' \@GET getUserHandler
--
-- putUser :: ('MonadRouter' m, 'Has' IntUserId req) => 'Handler' m req '[] ByteString
-- putUser = 'method' \@PUT
--           $ 'requestContentType' \@"application/json"
--           $ 'jsonRequestBody' \@User
--           $ putUserHandler
--
-- deleteUser :: ('MonadRouter' m, 'Has' IntUserId req) => 'Handler' m req '[] ByteString
-- deleteUser = 'method' \@DELETE deleteUserHandler
-- @
--
--
-- $running
--
-- Routable handlers can be converted to a regular function using
-- 'runRoute':
--
-- @
-- runRoute :: Monad m => 'Handler' (RouterT m) '[] res ByteString -> ('Wai.Request' -> m 'Wai.Response')
-- @
--
-- This function converts a WebGear handler to a function from
-- 'Wai.Request' to 'Wai.Response' in a monadic context @m@. Then it
-- is trivial to convert that to a WAI 'Wai.Application' and run it as a
-- warp server:
--
-- @
-- application :: 'Wai.Application'
-- application req respond = 'runRoute' allRoutes req >>= respond
--
-- main :: IO ()
-- main = Warp.run 3000 application
-- @
--
