module Properties.Trait.Path
  ( tests
  ) where

import Control.Monad.State.Strict (evalState)
import Data.String (fromString)
import Network.Wai (defaultRequest)
import Test.QuickCheck (Property, allProperties, property, (=/=), (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import WebGear.Middlewares.Path
import WebGear.Trait
import WebGear.Types


prop_pathMatch :: Property
prop_pathMatch = property $ \h ->
  let
    rest = ["foo", "bar"]
    req = linkzero $ defaultRequest { pathInfo = h:rest }
  in
    case evalState (tryLink (Path @"a") req) (PathInfo $ h:rest) of
      Right _ -> h === "a"
      Left _  -> h =/= "a"

prop_pathVarMatch :: Property
prop_pathVarMatch = property $ \(n :: Int) ->
  let
    rest = ["foo", "bar"]
    req = defaultRequest { pathInfo = fromString (show n):rest }
  in
    case evalState (tryLink (PathVar @"tag" @Int) (linkzero req)) (PathInfo $ pathInfo req) of
      Right n' -> n' === n
      Left _   -> property False

prop_pathVarParseError :: Property
prop_pathVarParseError = property $ \(p, ps) ->
  let
    p' = "test-" <> p
    req = defaultRequest { pathInfo = p':ps }
  in
    case evalState (tryLink (PathVar @"tag" @Int) (linkzero req)) (PathInfo $ pathInfo req) of
      Right _    -> property False
      Left e -> e === PathVarParseError ("could not parse: `" <> p' <> "' (input does not start with a digit)")


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Path" $allProperties
