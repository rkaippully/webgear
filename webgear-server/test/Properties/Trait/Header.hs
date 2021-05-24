module Properties.Trait.Header
  ( tests
  ) where

import Data.Functor.Identity (runIdentity)
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai (defaultRequest)
import Test.QuickCheck (Property, allProperties, counterexample, property, (.&&.), (=/=), (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import WebGear.Middlewares.Header
import WebGear.Trait
import WebGear.Types


prop_headerParseError :: Property
prop_headerParseError = property $ \hval ->
  let
    hval' = "test-" <> hval
    req = linkzero $ defaultRequest { requestHeaders = [("foo", encodeUtf8 hval')] }
  in
    case runIdentity (tryLink (Header' :: Header "foo" Int) req) of
      Right v    ->
        counterexample ("Unexpected result: " <> show v) (property False)
      Left e ->
        e === Right (HeaderParseError $ "could not parse: `" <> hval' <> "' (input does not start with a digit)")

prop_headerParseSuccess :: Property
prop_headerParseSuccess = property $ \(n :: Int) ->
  let
    req = linkzero $ defaultRequest { requestHeaders = [("foo", fromString $ show n)] }
  in
    case runIdentity (tryLink (Header' :: Header "foo" Int) req) of
      Right n'   -> n === n'
      Left e ->
        counterexample ("Unexpected result: " <> show e) (property False)

prop_headerMatch :: Property
prop_headerMatch = property $ \v ->
  let
    req = linkzero $ defaultRequest { requestHeaders = [("foo", v)] }
  in
    case runIdentity (tryLink (HeaderMatch' :: HeaderMatch "foo" "bar") req) of
      Right _           -> v === "bar"
      Left Nothing  ->
        counterexample "Unexpected result: Nothing" (property False)
      Left (Just e) ->
        expectedHeader e === "bar" .&&. actualHeader e =/= "bar"


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Header" $allProperties
