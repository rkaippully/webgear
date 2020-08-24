module Properties.Trait.Params
  ( tests
  ) where

import Data.Functor.Identity (runIdentity)
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai (defaultRequest, queryString)
import Test.QuickCheck (Property, allProperties, counterexample, property, (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import WebGear.Middlewares.Params
import WebGear.Trait


prop_headerParseError :: Property
prop_headerParseError = property $ \hval ->
  let
    hval' = "test-" <> hval
    req = defaultRequest { queryString = [("foo", Just $ encodeUtf8 hval')] }
  in
    case runIdentity (toAttribute @(QueryParam "foo" Int) req) of
      Proof _ v    ->
        counterexample ("Unexpected result: " <> show v) (property False)
      Refutation e ->
        e === Right (ParamParseError $ "could not parse: `" <> hval' <> "' (input does not start with a digit)")

prop_headerParseSuccess :: Property
prop_headerParseSuccess = property $ \(n :: Int) ->
  let
    req = defaultRequest { queryString = [("foo", Just $ fromString $ show n)] }
  in
    case runIdentity (toAttribute @(QueryParam "foo" Int) req) of
      Proof _ n'   -> n === n'
      Refutation e ->
        counterexample ("Unexpected result: " <> show e) (property False)


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Params" $allProperties
