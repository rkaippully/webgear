module WebGear.Route
  ( RouterT
  , unRoute
  ) where

import Control.Arrow
import Data.Version
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Paths_webgear_server (version)
import WebGear.Trait
import WebGear.Types


type RouterT m = ExceptT (Maybe (First Wai.Response)) m

unRoute :: Monad m
        => Handler (RouterT m) '[] res LByteString
        -> (Wai.Request -> m Wai.Response)
unRoute route req = either (maybe notFoundResponse getFirst) identity <$> runExceptT f
  where
    f = do
      res <- runKleisli (addServerHeader route) (linkzero $ Request req)
      pure $ waiResponse $ unlink res

notFoundResponse :: Wai.Response
notFoundResponse = Wai.responseLBS HTTP.notFound404 [serverHeader] "Not Found"

addServerHeader :: Monad m => ResponseMiddleware m '[] res '[] a
addServerHeader handler = Kleisli $
  runKleisli handler >=> pure . linkzero . addResponseHeader serverHeader . unlink

serverHeader :: HTTP.Header
serverHeader = (HTTP.hServer, fromString $ "WebGear/" ++ showVersion version)
