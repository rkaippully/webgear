module WebGear.Trait.Body
  ( JSONRequestBody
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (fromChunks)
import Data.Kind (Type)
import Data.Tagged (Tagged (..))

import WebGear.Trait (Trait (..))
import WebGear.Types (Request, requestBodyNextChunk)
import WebGear.Util (takeWhileM)


-- | A 'Trait' for converting a JSON request body into a Haskell object.
data JSONRequestBody (t :: Type)

instance (FromJSON t, MonadIO m) => Trait (JSONRequestBody t) Request m where
  type Val (JSONRequestBody t) Request = t

  check :: Request -> m (Maybe (Tagged (JSONRequestBody t) Request, t))
  check r = do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ requestBodyNextChunk r
    pure $ (,) (Tagged r) <$> decode (fromChunks chunks)
