module API.Handlers (createUser, listUsers, getUser, deleteUser) where

import Servant

import API.Data

exampleUser :: User
exampleUser =
    User
        { name = "foo"
        , email = "bar"
        }

createUser :: CreateUserReq -> Handler User
createUser _ = pure exampleUser

listUsers :: Handler [User]
listUsers = pure [exampleUser]

getUser :: Integer -> Handler User
getUser _ = pure exampleUser

deleteUser :: Integer -> Handler SuccessMsg
deleteUser _ = pure SuccessMsg{msg = "user deleted"}
