module Properties.Trait.Header
  ( tests
  ) where

import Data.Functor.Identity (runIdentity)
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai (defaultRequest, requestHeaders)
import Test.QuickCheck (Property, allProperties, counterexample, property, (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import WebGear.Trait
import WebGear.Trait.Header


prop_headerParseError :: Property
prop_headerParseError = property $ \hval ->
  let
    hval' = "test-" <> hval
    req = defaultRequest { requestHeaders = [("foo", encodeUtf8 hval')] }
  in
    case runIdentity (check @(Header "foo" Int) req) of
      CheckFail e      ->
        e === HeaderParseError ("could not parse: `" <> hval' <> "' (input does not start with a digit)")
      CheckSuccess _ v ->
        counterexample ("Unexpected result: " <> show v) (property False)

prop_headerParseSuccess :: Property
prop_headerParseSuccess = property $ \(n :: Int) ->
  let
    req = defaultRequest { requestHeaders = [("foo", fromString $ show n)] }
  in
    case runIdentity (check @(Header "foo" Int) req) of
      CheckFail e       ->
        counterexample ("Unexpected result: " <> show e) (property False)
      CheckSuccess _ n' -> n === n'


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Header" $allProperties
