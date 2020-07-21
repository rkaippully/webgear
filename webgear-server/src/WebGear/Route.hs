module WebGear.Route
  ( RouterT
  , MonadRouter (..)
  , runRoute
  ) where

import Control.Applicative (Alternative)
import Control.Arrow (Kleisli (..))
import Control.Monad (MonadPlus (..), (>=>))
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (fromList)
import Data.Semigroup (First (..))
import Data.String (fromString)
import Data.Version (showVersion)
import Network.HTTP.Types (Header, hServer, notFound404)

import Paths_webgear_server (version)
import WebGear.Trait (linkzero, unlink)
import WebGear.Types (Handler, Request (..), Response (..), ResponseMiddleware, addResponseHeader,
                      waiResponse)

import qualified Network.Wai as Wai


type RouterT m = ExceptT (Maybe (First (Response ByteString))) m

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

runRoute :: Monad m
         => Handler (RouterT m) '[] res ByteString
         -> (Wai.Request -> m Wai.Response)
runRoute route req = waiResponse . either (maybe notFoundResponse getFirst) id <$> runExceptT f
  where
    f = do
      res <- runKleisli (addServerHeader route) (linkzero $ Request req)
      pure $ unlink res

notFoundResponse :: Response ByteString
notFoundResponse = Response
  { respStatus  = notFound404
  , respHeaders = fromList [serverHeader]
  , respBody    = Just "Not Found"
  }

addServerHeader :: Monad m => ResponseMiddleware m '[] res '[] a
addServerHeader handler = Kleisli $
  runKleisli handler >=> pure . linkzero . addResponseHeader serverHeader . unlink

serverHeader :: Header
serverHeader = (hServer, fromString $ "WebGear/" ++ showVersion version)
