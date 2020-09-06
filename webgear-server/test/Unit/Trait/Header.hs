-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module Unit.Trait.Header
  ( tests
  ) where

import Network.Wai (defaultRequest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import WebGear.Middlewares.Header
import WebGear.Trait
import WebGear.Types


testMissingHeaderFails :: TestTree
testMissingHeaderFails = testCase "Missing header fails Header trait" $ do
  let req = defaultRequest { requestHeaders = [] }
  toAttribute @(Header "foo" Int) req >>= \case
    Found _      -> assertFailure "unexpected success"
    NotFound e -> e @?= Left HeaderNotFound

testHeaderMatchPositive :: TestTree
testHeaderMatchPositive = testCase "Header match: positive" $ do
  let req = defaultRequest { requestHeaders = [("foo", "bar")] }
  toAttribute @(HeaderMatch "foo" "bar") req >>= \case
    Found _      -> pure ()
    NotFound e -> assertFailure $ "Unexpected result: " <> show e

testHeaderMatchMissingHeader :: TestTree
testHeaderMatchMissingHeader = testCase "Header match: missing header" $ do
  let req = defaultRequest { requestHeaders = [] }
  toAttribute @(HeaderMatch "foo" "bar") req >>= \case
    Found _      -> assertFailure "unexpected success"
    NotFound e -> e @?= Nothing

tests :: TestTree
tests = testGroup "Trait.Header" [ testMissingHeaderFails
                                 , testHeaderMatchPositive
                                 , testHeaderMatchMissingHeader
                                 ]
