{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Control.Arrow (returnA)

import Opaleye hiding (Query)
import Opaleye as Db
import Data.Profunctor.Product
import Data.Profunctor.Product.TH

import Database.PostgreSQL.Simple as PGS
import Control.Monad.IO.Class (liftIO)
--
-- example datatype
--
type UserId = Int
data User' uid name pw = User
  { userId   :: uid
  , userName :: name
  , userPassword :: pw
  }
  deriving (Show, Eq, Generic)

type User = User' UserId Text Text
type UserColumn = User' (Column PGInt4) (Column PGText) (Column PGText)
$(makeAdaptorAndInstance "pUser" ''User')

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

userTable :: Table UserColumn UserColumn
userTable
  = Table "userTable"
    $ pUser User { userId = required "id"
                 , userName = required "name"
                 , userPassword = required "password"
                 }

chris :: User
chris = User 1 "Chris" "xx"

--
-- servant type level api
-- todo:
--   -> how to implement partial update (PATCH) on type level?
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


pgText :: Text -> Column PGText
pgText = pgString . T.unpack

getUsers :: PGS.Connection -> EitherT ServantErr IO [User]
getUsers con
  = liftIO $ Db.runQuery con selectUser

selectUser :: Db.Query UserColumn
selectUser = queryTable userTable

-- todo: how to get unique user not list?
selectUserById :: UserId -> Db.Query UserColumn
selectUserById id
  = proc () -> do
      user@(User uid _ _) <- selectUser -< ()
      restrict -< uid .== pgInt4 id
      returnA -< user

getUser :: PGS.Connection -> UserId -> EitherT ServantErr IO User
getUser con uid
  = do
    ux <- liftIO . (Db.runQuery con) . selectUserById $ uid
    case ux of
      []    -> left err404
      (x:_) -> return x

deleteUser :: PGS.Connection -> UserId -> EitherT ServantErr IO UserId
deleteUser con did
  = do
    liftIO $ Db.runDelete con userTable (\u -> (userId u) .== pgInt4 did)
    return did

-- todo: implement
updateUser :: PGS.Connection -> UserId -> User -> EitherT ServantErr IO User
updateUser con uid @user(User _ name pw) = left err404
--  = liftIO $ runUpdate con userTable updatedUser (\u -> (userId u) .== pgInt4 uid)
--    return user
--    where
--     updatedUser _
--       = User { userId = pgInt4 uid
--              , userName = pgText name
--              , userPassword = pgText pw
--              }

createUser :: PGS.Connection -> User -> EitherT ServantErr IO User
createUser con user@(User uid name pw)
  = do
    liftIO . (runInsert con userTable)
      $ User { userId = pgInt4 uid
             , userName = pgText name
             , userPassword = pgText pw
             }
    return user

api :: PGS.Connection -> Server UserAPI
api con = getUsers con
    :<|> getUser con
    :<|> deleteUser con
    :<|> updateUser con
    :<|> createUser con


proxy :: Proxy UserAPI
proxy = Proxy

app :: PGS.Connection -> Application
app con = serve proxy $ api con

main :: IO ()
main = do
  con <- PGS.connect PGS.defaultConnectInfo
                      { PGS.connectUser = "haskell"
                      , PGS.connectPassword = "test_password"
                      , PGS.connectDatabase = "chris"
                      }
  run 8081 $ app con
