module API.GalleyInternal where

import qualified Data.Aeson as Aeson
import Data.String.Conversions (cs)
import qualified Data.Vector as Vector
import GHC.Stack
import Testlib.Prelude

putTeamMember :: (HasCallStack, MakesValue user, MakesValue team) => user -> team -> Int -> App Response
putTeamMember user team perms = do
  uid <- objId user
  tid <- asString team
  req <-
    baseRequest
      ownDomain
      Galley
      Unversioned
      ("/i/teams/" <> tid <> "/members")

  submit
    "PUT"
    $ addJSONObject
      [ "member"
          .= object
            [ "user" .= uid,
              "permissions"
                .= object
                  [ "self" .= perms,
                    "copy" .= perms
                  ]
            ]
      ]
      req

getTeamFeature :: HasCallStack => String -> String -> App Response
getTeamFeature featureName tid = do
  req <- baseRequest ownDomain Galley Unversioned $ joinHttpPath ["i", "teams", tid, "features", featureName]
  submit "GET" $ req

getFederationStatus ::
  ( HasCallStack,
    MakesValue user
  ) =>
  user ->
  [String] ->
  App Response
getFederationStatus user domains =
  let domainList = Aeson.Array (Vector.fromList $ Aeson.String . cs <$> domains)
   in do
        uid <- objId user
        req <- baseRequest user Galley Unversioned $ joinHttpPath ["i", "federation-status"]
        submit
          "GET"
          ( req
              & zUser uid
              & addJSONObject ["domains" .= domainList]
          )
