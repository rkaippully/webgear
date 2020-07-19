module WebGear.Trait.Body
  ( JSONRequestBody
  ) where

import           Data.Aeson           (FromJSON)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import           WebGear.Trait
import           WebGear.Types


-- | A 'Trait' for converting a JSON request body into a Haskell object.
data JSONRequestBody (t :: Type)

instance (FromJSON t, MonadIO m) => Trait (JSONRequestBody t) Request m where
  type Val (JSONRequestBody t) Request = t

  check :: Tagged (JSONRequestBody t) Request -> m (Maybe (Request, t))
  check (Tagged r) = do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ requestBodyNextChunk r
    pure $ (,) r <$> Aeson.decode (LBS.fromChunks chunks)
