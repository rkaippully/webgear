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

import WebGear.Middlewares.Path
import WebGear.Trait


prop_pathMatch :: Property
prop_pathMatch = property $ \h ->
  let
    rest = ["foo", "bar"]
    req = defaultRequest { pathInfo = h:rest }
  in
    case runIdentity (toAttribute @(Path "a") req) of
      Proof req' _ -> h === "a" .&&. pathInfo req' === rest
      Refutation _ -> h =/= "a"

prop_pathVarMatch :: Property
prop_pathVarMatch = property $ \(n :: Int) ->
  let
    rest = ["foo", "bar"]
    req = defaultRequest { pathInfo = fromString (show n):rest }
  in
    case runIdentity (toAttribute @(PathVar "tag" Int) req) of
      Proof req' n' -> n' === n .&&. pathInfo req' === rest
      Refutation _  -> property False

prop_pathVarParseError :: Property
prop_pathVarParseError = property $ \(p, ps) ->
  let
    p' = "test-" <> p
    req = defaultRequest { pathInfo = p':ps }
  in
    case runIdentity (toAttribute @(PathVar "tag" Int) req) of
      Proof _ _    -> property False
      Refutation e -> e === PathVarParseError ("could not parse: `" <> p' <> "' (input does not start with a digit)")


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Path" $allProperties
