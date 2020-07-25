{-|
Copyright        : (c) Raghu Kaippully, 2020
License          : MPL-2.0
Maintainer       : rkaippully@gmail.com

Traits related to HTTP body.
-}
module WebGear.Trait.Body
  ( JSONRequestBody
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, eitherDecode')
import Data.ByteString.Lazy (fromChunks)
import Data.Kind (Type)
import Data.Text (Text, pack)

import WebGear.Trait (CheckResult (..), Trait (..))
import WebGear.Types (Request, getRequestBodyChunk)
import WebGear.Util (takeWhileM)


-- | A 'Trait' for converting a JSON request body into a value.
data JSONRequestBody (t :: Type)

instance (FromJSON t, MonadIO m) => Trait (JSONRequestBody t) Request m where
  type Val (JSONRequestBody t) Request = t
  type Fail (JSONRequestBody t) Request = Text

  check :: Request -> m (CheckResult (JSONRequestBody t) Request)
  check r = do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ getRequestBodyChunk r
    pure $ case eitherDecode' (fromChunks chunks) of
             Left e  -> CheckFail (pack e)
             Right t -> CheckSuccess r t
