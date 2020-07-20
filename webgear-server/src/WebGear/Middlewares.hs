{- | Middlewares in mtl style.
-}
module WebGear.Middlewares
  ( ok
  , noContent
  , badRequest
  , notFound

  , module WebGear.Middlewares.Method
  , module WebGear.Middlewares.Path
  , module WebGear.Middlewares.Header
  , module WebGear.Middlewares.Body
  ) where

import WebGear.Middlewares.Body
import WebGear.Middlewares.Header
import WebGear.Middlewares.Method
import WebGear.Middlewares.Path
import WebGear.Trait (Linked, linkzero)
import WebGear.Types (Response (..))

import qualified Network.HTTP.Types as HTTP


-- | Respond with a 200 OK
ok :: Monad m => a -> m (Linked '[] (Response a))
ok = pure . linkzero . Response HTTP.ok200 mempty . Just

-- | Respond with a 400 Bad Request
badRequest :: Monad m => m (Linked '[] (Response a))
badRequest = pure $ linkzero $ Response HTTP.badRequest400 mempty Nothing

-- | Respond with a 404 NotFound
notFound :: Monad m => m (Linked '[] (Response a))
notFound = pure $ linkzero $ Response HTTP.notFound404 mempty Nothing

noContent :: (Monad m, IsString s) => m (Linked '[] (Response s))
noContent = pure $ linkzero $ Response HTTP.noContent204 mempty $ Just ""
