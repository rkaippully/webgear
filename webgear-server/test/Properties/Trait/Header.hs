module Properties.Trait.Header
  ( tests
  ) where

import Data.String (fromString)
import Network.Wai (defaultRequest, requestHeaders)
import Test.QuickCheck (Property, allProperties, counterexample, property)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, monitor)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import WebGear.Trait
import WebGear.Trait.Header


prop_missingHeaderFails :: Property
prop_missingHeaderFails = monadicIO $ do
  let req = defaultRequest { requestHeaders = [] }
  check @(Header "foo" Int) req >>= \case
    CheckSuccess _ _ -> monitor (counterexample "Unexpected success") >> assert False
    CheckFail _      -> assert True

prop_headerParseError :: Property
prop_headerParseError = monadicIO $ do
  let req = defaultRequest { requestHeaders = [("foo", "bar")] }
  check @(Header "foo" Int) req >>= \case
    CheckFail e      -> do
      monitor (counterexample $ "Unexpected response: " <> show e)
      assert (e == HeaderParseError "could not parse: `bar' (input does not start with a digit)")
    CheckSuccess _ v -> do
      monitor (counterexample $ "Unexpected response: " <> show v)
      assert False

prop_headerParseSuccess :: Property
prop_headerParseSuccess = property $ \n -> monadicIO $ do
  let req = defaultRequest { requestHeaders = [("foo", fromString $ show (n :: Int))] }
  check @(Header "foo" Int) req >>= \case
    CheckFail e       -> do
      monitor (counterexample $ "Unexpected response: " <> show e)
      assert False
    CheckSuccess _ n' -> do
      monitor (counterexample $ "Unexpected response: " <> show n <> " vs " <> show n')
      assert (n == n')


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Header" $allProperties
