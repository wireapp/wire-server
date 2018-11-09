{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Web.SCIM.Server
import           Web.SCIM.Server.Mock
import           Web.SCIM.Schema.Meta hiding (meta)
import           Web.SCIM.Schema.Common as Common
import           Web.SCIM.Schema.ResourceType hiding (name)
import           Web.SCIM.Schema.User as User
import           Web.SCIM.Schema.User.Name
import           Web.SCIM.Schema.User.Email as E
import           Web.SCIM.Capabilities.MetaSchema as MetaSchema

import           Data.Time
import           Network.Wai.Handler.Warp
import           Network.URI.Static
import qualified STMContainers.Map as STMMap
import           Control.Monad.STM (atomically)
import           Text.Email.Validate

main :: IO ()
main = do
  storage <- TestStorage <$> mkUserDB <*> STMMap.newIO
  run 9000 (app MetaSchema.empty (nt storage))

-- | Create a UserDB with a single user:
--
-- @
-- UserID: sample-user
-- @
mkUserDB :: IO UserStorage
mkUserDB = do
  db <- STMMap.newIO
  now <- getCurrentTime
  let meta = Meta
        { resourceType = UserResource
        , created = now
        , lastModified = now
        , version = Weak "0" -- we don't support etags
        , location = Common.URI [relativeReference|/Users/sample-user|]
        }
  -- Note: Okta required at least one email, 'active', 'name.familyName',
  -- and 'name.givenName'. We might want to be able to express these
  -- constraints in code (and in the schema we serve) without turning this
  -- library into something that is Okta-specific.
  let email = Email
        { E.typ = Just "work"
        , E.value = maybe (error "couldn't parse email") EmailAddress2
                      (emailAddress "elton@wire.com")
        , E.primary = Nothing
        }
  let user = User.empty
        { userName = "elton"
        , name = Just Name
            { formatted = Just "Elton John"
            , familyName = Just "John"
            , givenName = Just "Elton"
            , middleName = Nothing
            , honorificPrefix = Nothing
            , honorificSuffix = Nothing
            }
        , active = Just True
        , emails = Just [email]
        }
  atomically $ STMMap.insert (WithMeta meta (WithId "elton" user)) "elton" db
  pure db
