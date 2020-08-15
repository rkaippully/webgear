module Properties.Trait.Method
  ( tests
  ) where

import Data.Functor.Identity (runIdentity)
import Network.HTTP.Types (StdMethod (..), methodGet, renderStdMethod)
import Network.Wai (defaultRequest, requestMethod)
import Test.QuickCheck (Arbitrary (arbitrary), Property, allProperties, elements, property, (.&&.),
                        (=/=), (===))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import WebGear.Trait
import WebGear.Trait.Method


newtype MethodWrapper = MethodWrapper StdMethod
  deriving stock (Show)

instance Arbitrary MethodWrapper where
  arbitrary = elements $ MethodWrapper <$> [minBound..maxBound]

prop_methodMatch :: Property
prop_methodMatch = property $ \(MethodWrapper v) ->
  let
    req = defaultRequest { requestMethod = renderStdMethod v }
  in
    case runIdentity (check @(Method GET) req) of
      CheckSuccess _ v' -> v === GET .&&. v' === methodGet
      CheckFail e       ->
        expectedMethod e === methodGet .&&. actualMethod e =/= methodGet


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Method" $allProperties