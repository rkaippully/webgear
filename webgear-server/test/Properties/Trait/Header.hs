module Properties.Trait.Header
  ( tests
  ) where

import Data.Functor.Identity (runIdentity)
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai (defaultRequest, requestHeaders)
import Test.QuickCheck (Property, allProperties, counterexample, property, (.&&.), (=/=), (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import WebGear.Middlewares.Header
import WebGear.Trait


prop_headerParseError :: Property
prop_headerParseError = property $ \hval ->
  let
    hval' = "test-" <> hval
    req = defaultRequest { requestHeaders = [("foo", encodeUtf8 hval')] }
  in
    case runIdentity (toAttribute @(Header "foo" Int) req) of
      Proof _ v    ->
        counterexample ("Unexpected result: " <> show v) (property False)
      Refutation e ->
        e === HeaderParseError ("could not parse: `" <> hval' <> "' (input does not start with a digit)")

prop_headerParseSuccess :: Property
prop_headerParseSuccess = property $ \(n :: Int) ->
  let
    req = defaultRequest { requestHeaders = [("foo", fromString $ show n)] }
  in
    case runIdentity (toAttribute @(Header "foo" Int) req) of
      Proof _ n'   -> n === n'
      Refutation e ->
        counterexample ("Unexpected result: " <> show e) (property False)

prop_headerMatch :: Property
prop_headerMatch = property $ \v ->
  let
    req = defaultRequest { requestHeaders = [("foo", v)] }
  in
    case runIdentity (toAttribute @(HeaderMatch "foo" "bar") req) of
      Proof _ _    -> v === "bar"
      Refutation e ->
        expectedHeader e === "bar" .&&. actualHeader e =/= Nothing .&&. actualHeader e =/= Just "bar"


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Header" $allProperties
