{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}

module Servant where

import Control.Monad.Except (ExceptT, MonadError, mapExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Proxy (Proxy (..))
import Network.Wai
import Servant.API
import Servant.Server

import Model

type UserAPI = "v1" :> "users" :> Capture "userId" Int :> Get '[JSON] User
               :<|> "v1" :> "users" :> Capture "userId" Int :> ReqBody '[JSON] User :> Put '[JSON] User
               :<|> "v1" :> "users" :> Capture "userId" Int :> Verb DELETE 204 '[JSON] NoContent

application :: UserStore -> Application
application store = serve userAPI $ hoistServer userAPI toHandler server
  where
    toHandler :: UserHandler a -> Handler a
    toHandler = Handler . mapExceptT f

    f :: ReaderT UserStore IO (Either ServerError a) -> IO (Either ServerError a)
    f x = runReaderT x store

userAPI :: Proxy UserAPI
userAPI = Proxy

type UserHandler = ExceptT ServerError (ReaderT UserStore IO)

server :: ServerT UserAPI UserHandler
server = getUser :<|> putUser :<|> deleteUser

getUser :: ( MonadReader UserStore m
           , MonadError ServerError m
           , MonadIO m
           )
        => Int -> m User
getUser uid = do
  store <- ask
  lookupUser store (UserId uid) >>= \case
    Just user -> return user
    Nothing   -> throwError err404

putUser :: ( MonadReader UserStore m
           , MonadIO m
           )
        => Int -> User -> m User
putUser uid user = do
  let user' = user { userId = UserId uid }
  store <- ask
  addUser store user'
  return user'

deleteUser :: ( MonadReader UserStore m
              , MonadError ServerError m
              , MonadIO m
              )
           => Int -> m NoContent
deleteUser uid = do
  store <- ask
  found <- removeUser store (UserId uid)
  if found
    then return NoContent
    else throwError err404
