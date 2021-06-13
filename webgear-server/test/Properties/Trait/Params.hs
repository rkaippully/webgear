module Properties.Trait.Params
  ( tests
  ) where

import Data.Functor.Identity (runIdentity)
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai (defaultRequest)
import Test.QuickCheck (Property, allProperties, counterexample, property, (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import WebGear.Middlewares.Params
import WebGear.Trait
import WebGear.Types


prop_paramParseError :: Property
prop_paramParseError = property $ \hval ->
  let
    hval' = "test-" <> hval
    req = linkzero $ defaultRequest { queryString = [("foo", Just $ encodeUtf8 hval')] }
  in
    case runIdentity (tryLink (QueryParam' :: QueryParam "foo" Int) req) of
      Right v    ->
        counterexample ("Unexpected result: " <> show v) (property False)
      Left e ->
        e === Right (ParamParseError $ "could not parse: `" <> hval' <> "' (input does not start with a digit)")

prop_paramParseSuccess :: Property
prop_paramParseSuccess = property $ \(n :: Int) ->
  let
    req = linkzero $ defaultRequest { queryString = [("foo", Just $ fromString $ show n)] }
  in
    case runIdentity (tryLink (QueryParam' :: QueryParam "foo" Int) req) of
      Right n'   -> n === n'
      Left e ->
        counterexample ("Unexpected result: " <> show e) (property False)


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Params" $allProperties
