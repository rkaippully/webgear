{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

--{-# OPTIONS_GHC -ddump-splices #-}

module Entities where

import Data.Text (Text)
import Database.Groundhog ()
import Database.Groundhog.Core
import Database.Groundhog.TH
import GHC.Generics (Generic)


newtype PrimaryKey = PrimaryKey { unPrimaryKey :: Maybe Int }
  deriving stock (Eq, Show, Read)
  deriving newtype (PrimitivePersistField)

instance PersistField PrimaryKey where
  persistName (PrimaryKey x) = "PrimaryKey" ++ delim:persistName x
  toPersistValues (PrimaryKey x) = toPersistValues x
  fromPersistValues xs = do
    (pv, pvs) <- fromPersistValues xs
    pure (PrimaryKey pv, pvs)
  dbType proxy _ = dbType proxy (0 :: Int)

data User = User
  { userid   :: PrimaryKey
  , username :: Text
  , email    :: Text
  , password :: Text
  , bio      :: Maybe Text
  , image    :: Maybe Text
  }
  deriving (Generic)


mkPersist defaultCodegenConfig [groundhog|
- entity: User
  autoKey: null
  constructors:
    - name: User
      uniques:
        - name: UserId
          fields: [userid]
          type: primary
        - name: UserName
          fields: [username]
        - name: UserEmail
          fields: [email]
  keys:
    - name: UserId
      default: true
    - name: UserName
    - name: UserEmail
|]
