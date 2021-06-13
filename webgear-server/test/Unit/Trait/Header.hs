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
  let req = linkzero $ defaultRequest { requestHeaders = [] }
  tryLink (Header' :: Header "foo" Int) req >>= \case
    Right _ -> assertFailure "unexpected success"
    Left e  -> e @?= Left HeaderNotFound

testHeaderMatchPositive :: TestTree
testHeaderMatchPositive = testCase "Header match: positive" $ do
  let req = linkzero $ defaultRequest { requestHeaders = [("foo", "bar")] }
  tryLink (HeaderMatch' :: HeaderMatch "foo" "bar") req >>= \case
    Right _ -> pure ()
    Left e  -> assertFailure $ "Unexpected result: " <> show e

testHeaderMatchMissingHeader :: TestTree
testHeaderMatchMissingHeader = testCase "Header match: missing header" $ do
  let req = linkzero $ defaultRequest { requestHeaders = [] }
  tryLink (HeaderMatch' :: HeaderMatch "foo" "bar") req >>= \case
    Right _ -> assertFailure "unexpected success"
    Left e  -> e @?= Nothing

tests :: TestTree
tests = testGroup "Trait.Header" [ testMissingHeaderFails
                                 , testHeaderMatchPositive
                                 , testHeaderMatchMissingHeader
                                 ]
