{-# OPTIONS_GHC -Wno-deprecations #-}

module Properties.Trait.Body
  ( tests
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.String (fromString)
import Network.Wai (defaultRequest, requestBody)
import Test.QuickCheck (Property, allProperties, counterexample, property)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, monitor)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import WebGear.Middlewares.Body
import WebGear.Trait
import WebGear.Types


bodyToRequest :: (MonadIO m, Show a) => a -> m Request
bodyToRequest x = do
  body <- liftIO $ newIORef $ Just $ fromString $ show x
  let f = readIORef body >>= maybe (pure "") (\s -> writeIORef body Nothing >> pure s)
  return defaultRequest { requestBody = f }

prop_emptyRequestBodyFails :: Property
prop_emptyRequestBodyFails = monadicIO $ do
  req <- bodyToRequest ("" :: String)
  toAttribute @(JSONRequestBody Int) req >>= \case
    Found _    -> monitor (counterexample "Unexpected success") >> assert False
    NotFound _ -> assert True

prop_validBodyParses :: Property
prop_validBodyParses = property $ \n -> monadicIO $ do
  req <- bodyToRequest (n :: Integer)
  toAttribute @(JSONRequestBody Integer) req >>= \case
    Found n'   -> assert (n == n')
    NotFound _ -> assert False

prop_invalidBodyFails :: Property
prop_invalidBodyFails = property $ \n -> monadicIO $ do
  req <- bodyToRequest (n :: Integer)
  toAttribute @(JSONRequestBody String) req >>= \case
    Found _    -> assert False
    NotFound _ -> assert True


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Body" $allProperties
