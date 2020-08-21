-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Middlewares provided by WebGear.
--
module WebGear.Middlewares
  ( module WebGear.Middlewares.Method
  , module WebGear.Middlewares.Path
  , module WebGear.Middlewares.Header
  , module WebGear.Middlewares.Body
  , module WebGear.Middlewares.Auth.Basic
  ) where

import WebGear.Middlewares.Auth.Basic
import WebGear.Middlewares.Body
import WebGear.Middlewares.Header
import WebGear.Middlewares.Method
import WebGear.Middlewares.Path
