-- |
-- Copyright        : (c) Raghu Kaippully, 2020
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module Properties
  ( propertyTests
  ) where

import Test.Tasty (TestTree, testGroup)

import qualified Properties.Trait.Body as Body
import qualified Properties.Trait.Header as Header
import qualified Properties.Trait.Method as Method
import qualified Properties.Trait.Path as Path


propertyTests :: TestTree
propertyTests = testGroup "Property Tests" [ Body.tests
                                           , Header.tests
                                           , Method.tests
                                           , Path.tests
                                           ]
