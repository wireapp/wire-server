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

setTeamFeatureStatus :: (HasCallStack, MakesValue domain, MakesValue team) => domain -> team -> String -> String -> App ()
setTeamFeatureStatus domain team featureName status = do
  tid <- asString team
  req <- baseRequest domain Galley Unversioned $ joinHttpPath ["i", "teams", tid, "features", featureName]
  res <- submit "PATCH" $ req & addJSONObject ["status" .= status]
  res.status `shouldMatchInt` 200

setTeamFeatureLockStatus :: (HasCallStack, MakesValue domain, MakesValue team) => domain -> team -> String -> String -> App ()
setTeamFeatureLockStatus domain team featureName status = do
  tid <- asString team
  req <- baseRequest domain Galley Unversioned $ joinHttpPath ["i", "teams", tid, "features", featureName, status]
  res <- submit "PUT" $ req
  res.status `shouldMatchInt` 200

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

-- | https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/galley/#/galley/put_i_legalhold_whitelisted_teams__tid_
legalholdWhitelistTeam :: (HasCallStack, MakesValue uid, MakesValue tid) => tid -> uid -> App Response
legalholdWhitelistTeam tid uid = do
  tidStr <- asString tid
  req <- baseRequest uid Galley Unversioned $ joinHttpPath ["i", "legalhold", "whitelisted-teams", tidStr]
  submit "PUT" req

-- | https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/galley/#/galley/get_i_legalhold_whitelisted_teams__tid_
legalholdIsTeamInWhitelist :: (HasCallStack, MakesValue uid, MakesValue tid) => tid -> uid -> App Response
legalholdIsTeamInWhitelist tid uid = do
  tidStr <- asString tid
  req <- baseRequest uid Galley Unversioned $ joinHttpPath ["i", "legalhold", "whitelisted-teams", tidStr]
  submit "GET" req

-- | https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/galley/#/galley/get_i_teams__tid__features_legalhold
legalholdIsEnabled :: (HasCallStack, MakesValue tid, MakesValue uid) => tid -> uid -> App Response
legalholdIsEnabled tid uid = do
  tidStr <- asString tid
  baseRequest uid Galley Unversioned do joinHttpPath ["i", "teams", tidStr, "features", "legalhold"]
    >>= submit "GET"

generateVerificationCode :: (HasCallStack, MakesValue domain, MakesValue email) => domain -> email -> App ()
generateVerificationCode domain email = do
  res <- generateVerificationCode' domain email
  res.status `shouldMatchInt` 200

generateVerificationCode' :: (HasCallStack, MakesValue domain, MakesValue email) => domain -> email -> App Response
generateVerificationCode' domain email = do
  req <- baseRequest domain Brig Versioned "/verification-code/send"
  emailStr <- asString email
  submit "POST" $ req & addJSONObject ["email" .= emailStr, "action" .= "login"]
