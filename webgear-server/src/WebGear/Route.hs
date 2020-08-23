-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Types and functions to route HTTP requests.
module WebGear.Route
  ( RouterT
  , MonadRouter (..)
  , runRoute
  ) where

import Control.Applicative (Alternative)
import Control.Arrow (Kleisli (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (fromList)
import Data.Semigroup (First (..))
import Data.String (fromString)
import Data.Version (showVersion)
import Network.HTTP.Types (Header, hServer, notFound404)

import Paths_webgear_server (version)
import WebGear.Trait (link)
import WebGear.Types (Handler, Response (..), waiResponse)

import qualified Network.Wai as Wai


-- | The monad transformer stack for routing.
--
-- * The 'ExceptT' provides short-circuiting behaviour for
--   'rejectRoute' and 'failHandler'.
--
-- * In case of 'rejectRoute', a 'Nothing' value is returned and in
--   case of 'failHandler', a @Response ByteString@ is returned.
--
-- * The 'First' wrapper is provided to get instances of 'Alternative'
--   and 'MonadPlus' for 'RouterT'.
--
type RouterT m = ExceptT (Maybe (First (Response ByteString))) m

-- | HTTP request routing with short circuiting behavior.
class (Alternative m, MonadPlus m) => MonadRouter m where
  -- | Mark the current route as rejected, alternatives can be tried
  rejectRoute :: m a

  -- | Short-circuit the current handler and return a response
  failHandler :: Response ByteString -> m a

instance Monad m => MonadRouter (RouterT m) where
  rejectRoute :: RouterT m a
  rejectRoute = mzero

  failHandler :: Response ByteString -> RouterT m a
  failHandler = throwError . Just . First

-- | Convert a routable handler into a plain function.
--
-- This function is typically used to convert WebGear routes to a
-- 'Wai.Application'.
runRoute :: Monad m
         => Handler (RouterT m) '[] ByteString
         -> (Wai.Request -> m Wai.Response)
runRoute route req = waiResponse . addServerHeader . either (maybe notFoundResponse getFirst) id <$> runExceptT f
  where
    f = runKleisli route (link req)

    notFoundResponse :: Response ByteString
    notFoundResponse = Response
      { responseStatus  = notFound404
      , responseHeaders = fromList []
      , responseBody    = Just "Not Found"
      }

    addServerHeader :: Response ByteString -> Response ByteString
    addServerHeader r = r { responseHeaders = responseHeaders r <> fromList [serverHeader] }

    serverHeader :: Header
    serverHeader = (hServer, fromString $ "WebGear/" ++ showVersion version)
