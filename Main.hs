{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Either
import Data.Text (Text)
import Data.Text as T
import Servant
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson
import Control.Applicative
import Control.Monad

--
-- example datatype
--
type UserId = Int
data User = User
  { userId   :: UserId
  , userName :: Text
  , password :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON (Object o) 
    = User <$>
      o .: "id" <*>
      o .: "name" <*>
      o .: "password"
  parseJSON _         
    = mzero

instance ToJSON User where
  toJSON (User id name pw)
    = object [ "id" .= id  
             , "name" .= name
             , "password" .= pw
             ]

chris :: User
chris = User 1 "Chris" "xx"

--
-- servant type level api
-- todo:
--   -> how to implement partial update (PATCH) on type level?
--   -> how to implement huge api? Having a huge api-type ?
--   -> are there type-level combinators to modulirze bug apis?
--
type UserAPI 
     = "users" :> Get '[JSON] [User]     -- list users
  :<|> "users" :> Capture "id" UserId
               :> Get '[JSON] User       -- get user by id
  :<|> "users" :> Capture "id" UserId
               :> Delete '[JSON] UserId  -- delete user by id
  :<|> "users" :> Capture "id" UserId
               :> ReqBody '[JSON] User
               :> Put '[JSON] User       -- update user by id
  :<|> "users" :> ReqBody '[JSON] User
               :> Post '[JSON] User      -- create user
  
getUsers :: EitherT ServantErr IO [User]
getUsers = return [ chris ] 

getUser :: UserId -> EitherT ServantErr IO User
getUser uid = return chris

deleteUser :: UserId -> EitherT ServantErr IO UserId
deleteUser uid = left err404

updateUser :: UserId -> User -> EitherT ServantErr IO User
updateUser uid user = left err404

createUser :: User -> EitherT ServantErr IO User
createUser user = left err404


api :: Server UserAPI
api = getUsers
    :<|> getUser
    :<|> deleteUser
    :<|> updateUser
    :<|> createUser

proxy :: Proxy UserAPI
proxy = Proxy

app :: Application
app = serve proxy api

main :: IO ()
main = do
  run 8081 app
