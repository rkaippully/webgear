-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares related to HTTP methods.
module WebGear.Middlewares.Method
  ( Method (..)
  , IsStdMethod (..)
  , MethodMismatch (..)
  , method
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad ((>=>))
import Data.Proxy (Proxy (..))
import qualified Network.HTTP.Types as HTTP
import WebGear.Trait (Linked, Trait (..), probe, unlink)
import WebGear.Types (MonadRouter (..), Request, RequestMiddleware', requestMethod)


-- | A 'Trait' for capturing the HTTP method of a request
data Method (t :: HTTP.StdMethod) = Method

-- | Failure to match method against an expected value
data MethodMismatch = MethodMismatch
  { expectedMethod :: HTTP.Method
  , actualMethod   :: HTTP.Method
  }

instance (IsStdMethod t, Monad m) => Trait (Method t) ts Request m where
  type Attribute (Method t) Request = HTTP.StdMethod
  type Absence (Method t) Request = MethodMismatch

  tryLink :: Method t
          -> Linked ts Request
          -> m (Either MethodMismatch HTTP.StdMethod)
  tryLink _ r =
    let
      m = toStdMethod $ Proxy @t
      expected = HTTP.renderStdMethod m
      actual = requestMethod $ unlink r
    in
      pure $
        if expected == actual
        then Right m
        else Left $ MethodMismatch expected actual


-- | A typeclass to map a 'HTTP.StdMethod' from type level to term
-- level.
class IsStdMethod t where
  -- | Convert @t@ to term level.
  toStdMethod :: Proxy t -> HTTP.StdMethod

instance IsStdMethod HTTP.GET where
  toStdMethod = const HTTP.GET
instance IsStdMethod HTTP.POST where
  toStdMethod = const HTTP.POST
instance IsStdMethod HTTP.HEAD where
  toStdMethod = const HTTP.HEAD
instance IsStdMethod HTTP.PUT where
  toStdMethod = const HTTP.PUT
instance IsStdMethod HTTP.DELETE where
  toStdMethod = const HTTP.DELETE
instance IsStdMethod HTTP.TRACE where
  toStdMethod = const HTTP.TRACE
instance IsStdMethod HTTP.CONNECT where
  toStdMethod = const HTTP.CONNECT
instance IsStdMethod HTTP.OPTIONS where
  toStdMethod = const HTTP.OPTIONS
instance IsStdMethod HTTP.PATCH where
  toStdMethod = const HTTP.PATCH

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
method :: forall t m req a. (IsStdMethod t, MonadRouter m)
       => RequestMiddleware' m req (Method t:req) a
method handler = Kleisli $ probe Method >=> either (const rejectRoute) (runKleisli handler)
