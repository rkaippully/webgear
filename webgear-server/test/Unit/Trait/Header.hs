-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module Unit.Trait.Header
  ( tests
  ) where

import Network.Wai (defaultRequest, requestHeaders)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import WebGear.Trait
import WebGear.Trait.Header


testMissingHeaderFails :: TestTree
testMissingHeaderFails = testCase "Missing header fails Header trait" $ do
  let req = defaultRequest { requestHeaders = [] }
  check @(Header "foo" Int) req >>= \case
    CheckSuccess _ _ -> assertFailure "unexpected success"
    CheckFail e      -> e @?= HeaderNotFound

testHeaderMatchPositive :: TestTree
testHeaderMatchPositive = testCase "Header match: positive" $ do
  let req = defaultRequest { requestHeaders = [("foo", "bar")] }
  check @(HeaderMatch "foo" "bar") req >>= \case
    CheckSuccess _ v -> v @?= "bar"
    CheckFail e      -> assertFailure $ "Unexpected result: " <> show e

testHeaderMatchMissingHeader :: TestTree
testHeaderMatchMissingHeader = testCase "Header match: missing header" $ do
  let req = defaultRequest { requestHeaders = [] }
  check @(HeaderMatch "foo" "bar") req >>= \case
    CheckSuccess _ _ -> assertFailure "unexpected success"
    CheckFail e      -> e @?= HeaderMismatch { expectedHeader = "bar"
                                             , actualHeader = Nothing
                                             }

tests :: TestTree
tests = testGroup "Trait.Header" [ testMissingHeaderFails
                                 , testHeaderMatchPositive
                                 , testHeaderMatchMissingHeader
                                 ]
