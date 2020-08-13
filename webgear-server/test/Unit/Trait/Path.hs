-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module Unit.Trait.Path
  ( tests
  ) where

import Network.Wai (defaultRequest, pathInfo)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import WebGear.Trait
import WebGear.Trait.Path


testMissingPathVar :: TestTree
testMissingPathVar = testCase "PathVar match: missing variable" $ do
  let req = defaultRequest { pathInfo = [] }
  check @(PathVar "tag" Int) req >>= \case
    CheckSuccess _ _ -> assertFailure "unexpected success"
    CheckFail e      -> e @?= PathVarNotFound

tests :: TestTree
tests = testGroup "Trait.Path" [ testMissingPathVar ]
