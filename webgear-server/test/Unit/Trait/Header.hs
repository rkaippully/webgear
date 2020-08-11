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

testPositiveHeaderMatch :: TestTree
testPositiveHeaderMatch = testCase "Positive header match" $ do
  let req = defaultRequest { requestHeaders = [("foo", "bar")] }
  check @(HeaderMatch "foo" "bar") req >>= \case
    CheckFail e      -> assertFailure $ "Unexpected result: " <> show e
    CheckSuccess _ v -> v @?= "bar"

tests :: TestTree
tests = testGroup "Trait.Header" [ testMissingHeaderFails
                                 , testPositiveHeaderMatch
                                 ]
