-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module Unit
  ( unitTests
  ) where

import Test.Tasty (TestTree, testGroup)

import qualified Unit.Trait.Header as Header


unitTests :: TestTree
unitTests = testGroup "Unit Tests" [ Header.tests ]
