module API.GalleyInternal where

import qualified Data.Aeson as Aeson
import Data.String.Conversions (cs)
import qualified Data.Text as T
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

getTeamFeature ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  String ->
  String ->
  App Response
getTeamFeature domain featureName tid = do
  req <-
    baseRequest domain Galley Unversioned $
      joinHttpPath ["i", "teams", tid, "features", featureName]
  submit "GET" req

patchTeamFeatureStatus ::
  (HasCallStack, MakesValue domain, MakesValue team) =>
  domain ->
  team ->
  String ->
  FeatureStatus ->
  App ()
patchTeamFeatureStatus domain team featureName status = do
  tid <- asString team
  req <-
    baseRequest domain Galley Unversioned $
      joinHttpPath ["i", "teams", tid, "features", featureName]
  res <- submit "PATCH" $ addJSONObject ["status" .= show status] req
  res.status `shouldMatchInt` 200

failToPatchTeamFeatureStatus ::
  (HasCallStack, MakesValue domain, MakesValue team) =>
  domain ->
  team ->
  String ->
  FeatureStatus ->
  App ()
failToPatchTeamFeatureStatus domain team featureName status = do
  tid <- asString team
  req <-
    baseRequest domain Galley Unversioned $
      joinHttpPath ["i", "teams", tid, "features", featureName]
  res <- submit "PATCH" $ addJSONObject ["status" .= show status] req
  res.status `shouldMatchRange` (400, 499)

-- | An alias for 'patchTeamFeatureStatus'
setTeamFeatureStatus ::
  (HasCallStack, MakesValue domain, MakesValue team) =>
  domain ->
  team ->
  String ->
  FeatureStatus ->
  App ()
setTeamFeatureStatus = patchTeamFeatureStatus

putTeamFeatureStatus ::
  (HasCallStack, MakesValue domain, MakesValue team) =>
  domain ->
  team ->
  String ->
  WithStatusNoLock ->
  App ()
putTeamFeatureStatus domain team featureName status = do
  tid <- asString team
  body <- make status
  req <-
    baseRequest domain Galley Unversioned $
      joinHttpPath ["i", "teams", tid, "features", featureName]
  res <- submit "PUT" $ addJSON body req
  res.status `shouldMatchInt` 200

failToPutTeamFeatureStatus ::
  (HasCallStack, MakesValue domain, MakesValue team) =>
  domain ->
  team ->
  String ->
  WithStatusNoLock ->
  App ()
failToPutTeamFeatureStatus domain team featureName status = do
  tid <- asString team
  body <- make status
  req <-
    baseRequest domain Galley Unversioned $
      joinHttpPath ["i", "teams", tid, "features", featureName]
  res <- submit "PUT" $ addJSON body req
  res.status `shouldMatchRange` (400, 499)

data FeatureStatus = Disabled | Enabled
  deriving (Eq)

instance HasTests x => HasTests (FeatureStatus -> x) where
  mkTests m n s f x =
    mkTests m (n <> "[featureStatus=disabled]") s f (x Disabled)
      <> mkTests m (n <> "[featureStatus=enabled]") s f (x Enabled)

instance Show FeatureStatus where
  show = \case
    Disabled -> "disabled"
    Enabled -> "enabled"

instance ToJSON FeatureStatus where
  toJSON = Aeson.String . T.pack . show

data WithStatusNoLock = WithStatusNoLock
  { status :: FeatureStatus,
    ttl :: Word
  }

instance MakesValue WithStatusNoLock where
  make = pure . toJSON

instance ToJSON WithStatusNoLock where
  toJSON (WithStatusNoLock s t) =
    Aeson.object
      [ "status" .= s,
        "ttl" .= case t of
          0 -> "unlimited"
          n -> show n
      ]

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
