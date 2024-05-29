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

setTeamFeatureStatus :: (HasCallStack, MakesValue domain, MakesValue team) => domain -> team -> String -> String -> App Response
setTeamFeatureStatus domain team featureName status = do
  tid <- asString team
  req <- baseRequest domain Galley Unversioned $ joinHttpPath ["i", "teams", tid, "features", featureName]
  submit "PATCH" $ req & addJSONObject ["status" .= status]

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

legalholdWhitelistTeam :: (HasCallStack, MakesValue uid, MakesValue tid) => uid -> tid -> App Response
legalholdWhitelistTeam uid tid = do
  tidStr <- asString tid
  req <- baseRequest uid Galley Unversioned $ joinHttpPath ["i", "legalhold", "whitelisted-teams", tidStr]
  submit "PUT" req

legalholdIsTeamInWhitelist :: (HasCallStack, MakesValue uid, MakesValue tid) => uid -> tid -> App Response
legalholdIsTeamInWhitelist uid tid = do
  tidStr <- asString tid
  req <- baseRequest uid Galley Unversioned $ joinHttpPath ["i", "legalhold", "whitelisted-teams", tidStr]
  submit "GET" req

setTeamFeatureConfig :: (HasCallStack, MakesValue domain, MakesValue team, MakesValue featureName, MakesValue payload) => Versioned -> domain -> team -> featureName -> payload -> App Response
setTeamFeatureConfig versioned domain team featureName payload = do
  tid <- asString team
  fn <- asString featureName
  p <- make payload
  req <- baseRequest domain Galley versioned $ joinHttpPath ["teams", tid, "features", fn]
  submit "PUT" $ req & addJSON p
