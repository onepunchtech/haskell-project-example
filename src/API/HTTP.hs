{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.HTTP (
    runServer,
) where

import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import API.Data
import API.Handlers

type ExampleAPI =
    "users" :> Get '[JSON] [User]
        :<|> "users" :> ReqBody '[JSON] CreateUserReq :> Post '[JSON] User
        :<|> "users" :> Capture "userid" Integer :> Get '[JSON] User
        :<|> "users" :> Capture "userid" Integer :> Delete '[JSON] SuccessMsg

exampleAPI :: Proxy ExampleAPI
exampleAPI = Proxy

server :: Server ExampleAPI
server = listUsers :<|> createUser :<|> getUser :<|> deleteUser

app :: Application
app = serve exampleAPI server

runServer :: IO ()
runServer = run 8081 app
