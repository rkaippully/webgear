{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Common where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson (Options (..), defaultOptions)
import Data.Char (isLower, isUpper, toLower)
import Data.Pool (Pool, withResource)
import Database.Esqueleto
import Database.Persist.Sqlite (withSqlitePool)
import Model.Entities (migrateAll)
import Relude

#if MIN_VERSION_esqueleto(3, 2, 0)
import Database.Esqueleto.Internal.Internal (Update)
#else
import Database.Esqueleto.Internal.Language (Update)
#endif


-- All DB operations run in this monad
type DBAction a = ReaderT SqlBackend IO a

withDBConnectionPool :: (Pool SqlBackend -> IO a) -> IO a
withDBConnectionPool f = runStdoutLoggingT $
  withSqlitePool "realworld.db" 20 $ \pool -> do
    withResource pool $ runSqlConn (runMigration migrateAll)
    liftIO $ f pool

-- An optional update operator
(=?.) :: (PersistEntity v, PersistField typ) => EntityField v typ -> Maybe typ -> Maybe (SqlExpr (Update v))
fld =?. mv = fmap (\v -> fld =. val v) mv

-- Aeson options to drop the entity name prefix from field names
dropPrefixOptions :: Options
dropPrefixOptions = defaultOptions
  { fieldLabelModifier = lowerFirst . dropWhile isLower
  }
  where
    lowerFirst :: String -> String
    lowerFirst (c:cs) | isUpper c = toLower c:cs
    lowerFirst s = s
