-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to HTTP methods.
module WebGear.Middlewares.Method
  ( method
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))

import WebGear.Route (MonadRouter (..))
import WebGear.Trait (linkplus)
import WebGear.Trait.Method (IsStdMethod, Method)
import WebGear.Types (RequestMiddleware)


-- | A middleware to check whether the request has a specified HTTP
-- method.
--
-- Typically this would be used with a type application such as:
--
-- > method @GET handler
--
-- It is also idiomatic to use the template haskell quasiquoter
-- 'WebGear.Middlewares.Path.match' in cases where both HTTP method
-- and path needs to be matched.
method :: forall t m req res a. (IsStdMethod t, MonadRouter m)
       => RequestMiddleware m req (Method t:req) res a
method handler = Kleisli $ linkplus @(Method t) >=> either (const rejectRoute) (runKleisli handler)
