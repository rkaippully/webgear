-- |
-- Copyright        : (c) Raghu Kaippully, 2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Handler serving static resources
module WebGear.Handlers.Static
  ( serveDir
  , serveFile
  ) where

import Control.Arrow (Kleisli (..))
import Control.Exception.Safe (catchIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (MonadState (..))
import Prelude hiding (readFile)
import System.FilePath (joinPath, takeFileName, (</>))

import WebGear.Types (Handler', MonadRouter (..), PathInfo (..), Response, notFound404, ok200,
                      setResponseHeader)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.Mime as Mime


serveDir :: (MonadRouter m, MonadIO m)
         => FilePath          -- ^ directory to serve
         -> Maybe FilePath    -- ^ index filename for the root directory
         -> Handler' m req LBS.ByteString
serveDir root index = Kleisli $ \_req -> do
  PathInfo restPath <- get
  case restPath of
    [] -> serveIndex
    ps -> serveFile $ root </> joinPath (Text.unpack <$> ps)
  where
    serveIndex = maybe
      (pure notFound404)
      (\f -> serveFile $ root </> f)
      index

serveFile :: (MonadRouter m, MonadIO m) => FilePath -> m (Response LBS.ByteString)
serveFile f = do
  contents <- liftIO $ (Just <$> LBS.readFile f) `catchIO` const (pure Nothing)
  let mimeType = Mime.defaultMimeLookup $ Text.pack $ takeFileName f
      mkResponse = pure . setResponseHeader "Content-Type" mimeType . ok200
  maybe (errorResponse notFound404) mkResponse contents
