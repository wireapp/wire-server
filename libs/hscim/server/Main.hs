{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.STM (atomically)
import Data.Time
import Network.URI.Static
import Network.Wai.Handler.Warp
import qualified StmContainers.Map as STMMap
import Text.Email.Validate
import Web.Scim.Capabilities.MetaSchema as MetaSchema
import Web.Scim.Schema.Common as Common
import Web.Scim.Schema.Meta hiding (meta)
import Web.Scim.Schema.ResourceType hiding (name)
import Web.Scim.Schema.Schema (Schema (User20))
import Web.Scim.Schema.User as User
import Web.Scim.Schema.User.Email as E
import Web.Scim.Schema.User.Name
import Web.Scim.Server
import Web.Scim.Server.Mock

main :: IO ()
main = do
  storage <- TestStorage <$> mkUserDB <*> STMMap.newIO
  run 9000 (app @Mock MetaSchema.empty (nt storage))

-- | Create a UserDB with a single user:
--
-- @
-- UserID: sample-user
-- @
mkUserDB :: IO UserStorage
mkUserDB = do
  db <- STMMap.newIO
  now <- getCurrentTime
  let meta =
        Meta
          { resourceType = UserResource,
            created = now,
            lastModified = now,
            version = Weak "0", -- we don't support etags
            location = Common.URI [relativeReference|/Users/sample-user|]
          }
  -- Note: Okta required at least one email, 'active', 'name.familyName',
  -- and 'name.givenName'. We might want to be able to express these
  -- constraints in code (and in the schema we serve) without turning this
  -- library into something that is Okta-specific.
  let email =
        Email
          { E.typ = Just "work",
            E.value =
              maybe
                (error "couldn't parse email")
                EmailAddress2
                (emailAddress "elton@wire.com"),
            E.primary = Nothing
          }
  let user =
        (User.empty [User20] "elton" NoUserExtra)
          { name =
              Just
                Name
                  { formatted = Just "Elton John",
                    familyName = Just "John",
                    givenName = Just "Elton",
                    middleName = Nothing,
                    honorificPrefix = Nothing,
                    honorificSuffix = Nothing
                  },
            active = Just True,
            emails = [email]
          }
  atomically $ STMMap.insert (WithMeta meta (WithId (Id 0) user)) (Id 0) db
  pure db
