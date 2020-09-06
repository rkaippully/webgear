-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module Unit.Trait.Path
  ( tests
  ) where

import Control.Monad.State (evalState)
import Network.Wai (defaultRequest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import WebGear.Middlewares.Path
import WebGear.Trait
import WebGear.Types


testMissingPathVar :: TestTree
testMissingPathVar = testCase "PathVar match: missing variable" $ do
  let req = defaultRequest { pathInfo = [] }
  case evalState (toAttribute @(PathVar "tag" Int) req) (PathInfo []) of
    Found _    -> assertFailure "unexpected success"
    NotFound e -> e @?= PathVarNotFound

tests :: TestTree
tests = testGroup "Trait.Path" [ testMissingPathVar ]
