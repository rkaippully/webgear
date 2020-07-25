{-|
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com

WebGear helps to build composable, type-safe HTTP API servers.
-}
module WebGear
  ( -- * Serving HTTP APIs
    -- $serving
    module WebGear.Middlewares
    , module WebGear.Route
    , module WebGear.Trait
    , module WebGear.Trait.Body
    , module WebGear.Trait.Header
    , module WebGear.Trait.Method
    , module WebGear.Trait.Path
    , module WebGear.Types
  ) where

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
-- An HTTP API server can be thought of as a function that takes an
-- request as input and produces a response as output; usually, in a
-- monadic context.
--
-- > server :: Monad m => Request -> m Response
--
-- Most APIs will require extracting some information from the
-- request, processing it and then producing a response.
--
-- As an example, let us say we want the API to retrieve user data via
-- a GET request to the URL containing user ID such as .
--
-- However, most HTTP APIs will have many endpoints. For example, an
-- API serving information about users could have an endpoint to
-- retrieve a user, another to update a user and one to delete a
-- user. Naturally, we would like to split the server to three
-- separate functions and compose them to produce a single server
-- function.
--
-- > getUser :: Monad m => Request -> m Response
-- > getUser = ...
-- >
-- > updateUser :: Monad m => Request -> m Response
-- > updateUser = ...
-- >
-- > deleteUser :: Monad m => Request -> m Response
-- > deleteUser = ...
-- >
-- > server :: Monad m => Request -> m Response
-- > server = getUser <|> updateUser <|> deleteUser
--
-- The '<|>' operator from 'Alternative' helps to pick
