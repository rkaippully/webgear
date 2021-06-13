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


bodyToRequest :: (MonadIO m, Show a) => a -> m (Linked '[] Request)
bodyToRequest x = do
  body <- liftIO $ newIORef $ Just $ fromString $ show x
  let f = readIORef body >>= maybe (pure "") (\s -> writeIORef body Nothing >> pure s)
  return $ linkzero $ defaultRequest { requestBody = f }

prop_emptyRequestBodyFails :: Property
prop_emptyRequestBodyFails = monadicIO $ do
  req <- bodyToRequest ("" :: String)
  tryLink (JSONBody @Int) req >>= \case
    Right _ -> monitor (counterexample "Unexpected success") >> assert False
    Left _  -> assert True

prop_validBodyParses :: Property
prop_validBodyParses = property $ \n -> monadicIO $ do
  req <- bodyToRequest (n :: Integer)
  tryLink JSONBody req >>= \case
    Right n' -> assert (n == n')
    Left _   -> assert False

prop_invalidBodyTypeFails :: Property
prop_invalidBodyTypeFails = property $ \n -> monadicIO $ do
  req <- bodyToRequest (n :: Integer)
  tryLink (JSONBody @String) req >>= \case
    Right _ -> assert False
    Left _  -> assert True


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Body" $allProperties
