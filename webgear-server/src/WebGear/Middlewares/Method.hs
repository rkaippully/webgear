module WebGear.Middlewares.Method
  ( method
  ) where

import Control.Arrow (Kleisli (..))

import WebGear.Route (MonadRouter (..))
import WebGear.Trait (linkplus)
import WebGear.Trait.Method (IsStdMethod, Method)
import WebGear.Types (RequestMiddleware)


method :: forall t m req res a. (IsStdMethod t, MonadRouter m) => RequestMiddleware m req (Method t:req) res a
method handler = Kleisli $ linkplus @(Method t) >=> maybe rejectRoute (runKleisli handler)
