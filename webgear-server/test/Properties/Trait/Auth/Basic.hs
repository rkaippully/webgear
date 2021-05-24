module Properties.Trait.Auth.Basic
  ( tests
  ) where

import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (elem)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity, runIdentity)
import Network.Wai (defaultRequest)
import Prelude hiding (elem)
import Test.QuickCheck (Discard (..), Property, allProperties, counterexample, property, (.&&.),
                        (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import WebGear.Middlewares.Auth.Basic
import WebGear.Middlewares.Auth.Util (AuthorizationHeader)
import WebGear.Middlewares.Header (Header' (Header'))
import WebGear.Trait
import WebGear.Types


prop_basicAuth :: Property
prop_basicAuth = property f
  where
    f (username, password)
      | ':' `elem` username = property Discard
      | otherwise =
          let
            hval = "Basic " <> encode (username <> ":" <> password)

            req :: Linked '[AuthorizationHeader "Basic"] Request
            req = fromRight undefined
                  $ runIdentity
                  $ probe Header'
                  $ linkzero
                  $ defaultRequest { requestHeaders = [("Authorization", hval)] }

            toBasicAttribute :: Credentials -> Identity (Either () Credentials)
            toBasicAttribute = pure . Right

            authCfg :: BasicAuth Identity () Credentials
            authCfg = BasicAuth'{..}
          in
            case runIdentity (tryLink authCfg req) of
              Right creds ->
                credentialsUsername creds === Username username
                .&&. credentialsPassword creds === Password password
              Left e  ->
                counterexample ("Unexpected failure: " <> show e) (property False)


-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Auth.Basic" $allProperties
