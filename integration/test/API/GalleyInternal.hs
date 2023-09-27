module API.GalleyInternal where

import Data.Aeson qualified as Aeson
import Data.String.Conversions (cs)
import Data.Vector qualified as Vector
import GHC.Stack
import Testlib.Prelude

putTeamMember :: (HasCallStack, MakesValue user, MakesValue team) => user -> team -> Int -> App Response
putTeamMember user team perms = do
  uid <- objId user
  tid <- asString team
  req <-
    baseRequest
      user
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

getTeamFeature :: (HasCallStack, MakesValue domain_) => domain_ -> String -> String -> App Response
getTeamFeature domain_ featureName tid = do
  req <- baseRequest domain_ Galley Unversioned $ joinHttpPath ["i", "teams", tid, "features", featureName]
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
        req <- baseRequest user Galley Unversioned $ joinHttpPath ["i", "federation-status"]
        submit
          "GET"
          $ req & addJSONObject ["domains" .= domainList]

deleteFederationDomain ::
  ( HasCallStack
  ) =>
  String ->
  App Response
deleteFederationDomain domain = do
  req <- rawBaseRequest OwnDomain Galley Unversioned $ joinHttpPath ["i", "federation", domain]
  submit "DELETE" req
