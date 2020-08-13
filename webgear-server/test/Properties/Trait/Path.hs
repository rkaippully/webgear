module Properties.Trait.Path
  ( tests
  ) where

import Data.Functor.Identity (runIdentity)
import Data.String (fromString)
import Network.Wai (defaultRequest, pathInfo)
import Test.QuickCheck (Property, allProperties, property, (.&&.), (=/=), (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

import WebGear.Trait
import WebGear.Trait.Path


prop_pathMatch :: Property
prop_pathMatch = property $ \h ->
  let
    rest = ["foo", "bar"]
    req = defaultRequest { pathInfo = h:rest }
  in
    case runIdentity (check @(Path "a") req) of
      CheckSuccess req' _ -> h === "a" .&&. pathInfo req' === rest
      CheckFail _         -> h =/= "a"

prop_pathVarMatch :: Property
prop_pathVarMatch = property $ \(n :: Int) ->
  let
    rest = ["foo", "bar"]
    req = defaultRequest { pathInfo = fromString (show n):rest }
  in
    case runIdentity (check @(PathVar "tag" Int) req) of
      CheckSuccess req' n' -> n' === n .&&. pathInfo req' === rest
      CheckFail _          -> property False

prop_pathVarParseError :: Property
prop_pathVarParseError = property $ \(p, ps) ->
  let
    p' = "test-" <> p
    req = defaultRequest { pathInfo = p':ps }
  in
    case runIdentity (check @(PathVar "tag" Int) req) of
      CheckSuccess _ _ -> property False
      CheckFail e      -> e === PathVarParseError ("could not parse: `" <> p' <> "' (input does not start with a digit)")


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Path" $allProperties
