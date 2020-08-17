{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Network.HTTP.Types (noContent204, notFound404)
import Network.Wai
import Web.Scotty

import Model


application :: UserStore -> IO Application
application store = scottyApp $ do
  get "/v1/users/:userId" $ getUser store
  put "/v1/users/:userId" $ putUser store
  delete "/v1/users/:userId" $ deleteUser store

getUser :: UserStore -> ActionM ()
getUser store = do
  uid <- param "userId"
  lookupUser store (UserId uid) >>= \case
    Just user -> json user
    Nothing   -> respondNotFound

putUser :: UserStore -> ActionM ()
putUser store = do
  uid <- param "userId"
  user <- jsonData
  let user' = user { userId = UserId uid }
  addUser store user'
  json user'

deleteUser :: UserStore -> ActionM ()
deleteUser store = do
  uid <- param "userId"
  found <- removeUser store (UserId uid)
  if found
    then status noContent204 >> raw ""
    else respondNotFound

respondNotFound :: ActionM ()
respondNotFound = do
  status notFound404
  text "Not found"
  finish
