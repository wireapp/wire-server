{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Stern.API
  ( start,
  )
where

import Brig.Types.Intra
import Control.Error
import Control.Lens ((^.))
import Data.Aeson hiding (Error, json)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (emptyArray)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import Data.Handle (Handle)
import Data.Id
import Data.Predicate
import Data.Proxy (Proxy (..))
import Data.Range
import qualified Data.Schema as S
import Data.String.Conversions (cs)
import Data.Swagger.Build.Api hiding (Response, def, min, response)
import qualified Data.Swagger.Build.Api as Doc
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import GHC.TypeLits (KnownSymbol)
import qualified Galley.Types.Teams.Intra as Team
import Imports hiding (head)
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Predicate hiding (Error, reason, setStatus)
import Network.Wai.Routing hiding (trace)
import Network.Wai.Utilities
import qualified Network.Wai.Utilities.Server as Server
import Network.Wai.Utilities.Swagger (document, mkSwaggerApi)
import Servant (NoContent (NoContent), ServerT, (:<|>) (..), (:>))
import qualified Servant
import qualified Servant.Server
import Stern.API.Predicates
import Stern.API.Routes
import qualified Stern.API.RoutesLegacy as RoutesLegacy
import Stern.App
import qualified Stern.Intra as Intra
import Stern.Options
import qualified Stern.Swagger as Doc
import Stern.Types
import System.Logger.Class hiding (Error, name, trace, (.=))
import Util.Options
import Wire.API.Connection
import Wire.API.Routes.Internal.Brig.Connection (ConnectionStatus)
import qualified Wire.API.Routes.Internal.Brig.EJPD as EJPD
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Team.Feature hiding (setStatus)
import qualified Wire.API.Team.Feature as Public
import Wire.API.Team.SearchVisibility
import qualified Wire.API.Team.SearchVisibility as Public
import Wire.API.User
import Wire.API.User.Search
import qualified Wire.Swagger as Doc

default (ByteString)

start :: Opts -> IO ()
start o = do
  e <- newEnv o
  s <- Server.newSettings (server e)
  Server.runSettingsWithShutdown s (servantApp e) Nothing
  where
    server :: Env -> Server.Server
    server e = Server.defaultServer (unpack $ stern o ^. epHost) (stern o ^. epPort) (e ^. applog) (e ^. metrics)

    servantApp :: Env -> Application
    servantApp e =
      Servant.serve
        ( Proxy
            @( SwaggerDocsAPI
                 :<|> SternAPIInternal
                 :<|> SternAPI
             )
        )
        ( swaggerDocsAPI
            :<|> servantSitemapInternal
            :<|> servantSitemap e
        )

-------------------------------------------------------------------------------
-- servant API

servantSitemap :: Stern.App.Env -> Servant.Server SternAPI
servantSitemap env = Servant.Server.hoistServer (Proxy @SternAPI) nt servantSitemap'
  where
    nt :: forall x. Stern.App.Handler x -> Servant.Server.Handler x
    nt m = Servant.Server.Handler . ExceptT $ do
      fmapL renderError <$> Stern.App.runAppT env (runExceptT m)

    renderError :: Error -> Servant.Server.ServerError
    renderError (Error code label message _) = Servant.Server.ServerError (statusCode code) (cs label) (cs message) []

servantSitemap' :: ServerT SternAPI Handler
servantSitemap' =
  Named @"suspend-user" suspendUser
    :<|> Named @"unsuspend-user" unsuspendUser
    :<|> Named @"get-users-by-email" usersByEmail
    :<|> Named @"get-users-by-phone" usersByPhone
    :<|> Named @"get-users-by-ids" usersByIds
    :<|> Named @"get-users-by-handles" usersByHandles
    :<|> Named @"get-user-connections" userConnections
    :<|> Named @"get-users-connections" usersConnections
    :<|> Named @"search-users" searchOnBehalf
    :<|> Named @"revoke-identity" revokeIdentity
    :<|> Named @"put-email" changeEmail
    :<|> Named @"put-phone" changePhone
    :<|> Named @"delete-user" deleteUser
    :<|> Named @"suspend-team" (setTeamStatusH Team.Suspended)
    :<|> Named @"unsuspend-team" (setTeamStatusH Team.Active)
    :<|> Named @"delete-team" deleteTeam
    :<|> Named @"ejpd-info" ejpdInfoByHandles
    :<|> Named @"head-user-blacklist" isUserKeyBlacklisted
    :<|> Named @"post-user-blacklist" addBlacklist
    :<|> Named @"delete-user-blacklist" deleteFromBlacklist
    :<|> Named @"get-team-info-by-member-email" getTeamInfoByMemberEmail
    :<|> Named @"get-team-info" getTeamInfo
    :<|> Named @"get-team-admin-info" getTeamAdminInfo
    :<|> Named @"get-route-legalhold-config" (mkFeatureGetRoute @LegalholdConfig)
    :<|> Named @"put-route-legalhold-config" (mkFeaturePutRouteTrivialConfigNoTTL @LegalholdConfig)
    :<|> Named @"get-route-sso-config" (mkFeatureGetRoute @SSOConfig)
    :<|> Named @"put-route-sso-config" (mkFeaturePutRouteTrivialConfigNoTTL @SSOConfig)
    :<|> Named @"get-route-search-visibility-available-config" (mkFeatureGetRoute @SearchVisibilityAvailableConfig)
    :<|> Named @"put-route-search-visibility-available-config" (mkFeaturePutRouteTrivialConfigNoTTL @SearchVisibilityAvailableConfig)
    :<|> Named @"get-route-validate-saml-emails-config" (mkFeatureGetRoute @ValidateSAMLEmailsConfig)
    :<|> Named @"put-route-validate-saml-emails-config" (mkFeaturePutRouteTrivialConfigNoTTL @ValidateSAMLEmailsConfig)
    :<|> Named @"get-route-digital-signatures-config" (mkFeatureGetRoute @DigitalSignaturesConfig)
    :<|> Named @"put-route-digital-signatures-config" (mkFeaturePutRouteTrivialConfigNoTTL @DigitalSignaturesConfig)
    :<|> Named @"get-route-file-sharing-config" (mkFeatureGetRoute @FileSharingConfig)
    :<|> Named @"put-route-file-sharing-config" (mkFeaturePutRouteTrivialConfigNoTTL @FileSharingConfig)
    :<|> Named @"get-route-classified-domains-config" (mkFeatureGetRoute @ClassifiedDomainsConfig)
    :<|> Named @"get-route-conference-calling-config" (mkFeatureGetRoute @ConferenceCallingConfig)
    :<|> Named @"put-route-conference-calling-config" (mkFeaturePutRouteTrivialConfigWithTTL @ConferenceCallingConfig)

{-

  mkFeatureGetRoute @AppLockConfig
  mkFeaturePutRoute @AppLockConfig

  mkFeatureGetRoute @MLSConfig
  mkFeaturePutRoute @MLSConfig

:<|> Named @"get-team-search-visibility" Intra.getSearchVisibility
:<|> Named @"get-team-invoice" getTeamInvoice
:<|> Named @"get-team-billing-info" getTeamBillingInfo
:<|> Named @"put-team-billing-info" updateTeamBillingInfo
:<|> Named @"post-team-billing-info" setTeamBillingInfo
:<|> Named @"get-consent-log" getConsentLog
:<|> Named @"get-user-metainfo" getUserData

-}

servantSitemapInternal :: Servant.Server SternAPIInternal
servantSitemapInternal =
  Named @"status" (pure Servant.NoContent)
    :<|> Named @"legacy-api-docs" serveLegacySwagger

-- | FUTUREWORK: remove this handler, the servant route, and module Stern.API.RoutesLegacy,
-- once we don't depend on swagger1.2 for stern any more.
serveLegacySwagger :: Text -> Servant.Server.Handler NoContent
serveLegacySwagger url =
  Servant.Server.Handler $
    throwE
      ( Servant.ServerError
          200
          mempty
          (encode $ RoutesLegacy.apiDocs (cs url))
          [("Content-Type", "application/json")]
      )

-----------------------------------------------------------------------------
-- Handlers

suspendUser :: UserId -> Handler NoContent
suspendUser uid = NoContent <$ Intra.putUserStatus Suspended uid

unsuspendUser :: UserId -> Handler NoContent
unsuspendUser uid = NoContent <$ Intra.putUserStatus Active uid

usersByEmail :: Email -> Handler [UserAccount]
usersByEmail = Intra.getUserProfilesByIdentity . Left

usersByPhone :: Phone -> Handler [UserAccount]
usersByPhone = Intra.getUserProfilesByIdentity . Right

usersByIds :: [UserId] -> Handler [UserAccount]
usersByIds = Intra.getUserProfiles . Left

usersByHandles :: [Handle] -> Handler [UserAccount]
usersByHandles = Intra.getUserProfiles . Right

ejpdInfoByHandles :: Maybe Bool -> [Handle] -> Handler EJPD.EJPDResponseBody
ejpdInfoByHandles (fromMaybe False -> includeContacts) handles = Intra.getEjpdInfo handles includeContacts

userConnections :: UserId -> Handler UserConnectionGroups
userConnections = fmap groupByStatus . Intra.getUserConnections

usersConnections :: [UserId] -> Handler [ConnectionStatus]
usersConnections = Intra.getUsersConnections . List

searchOnBehalf :: UserId -> Maybe T.Text -> Maybe Int32 -> Handler (SearchResult Contact)
searchOnBehalf
  uid
  (fromMaybe "" -> q)
  (fromMaybe (unsafeRange 10) . checked @Int32 @1 @100 . fromMaybe 10 -> s) =
    Intra.getContacts uid q (fromRange s)

revokeIdentity :: Maybe Email -> Maybe Phone -> Handler NoContent
revokeIdentity mbe mbp = NoContent <$ (Intra.revokeIdentity =<< doubleMaybeToEither "email, phone" mbe mbp)

changeEmail :: UserId -> Maybe Bool -> EmailUpdate -> Handler NoContent
changeEmail = undefined --  uid validate upd = NoContent <$ Intra.changeEmail uid (fromMaybe False upd) validate

changePhone :: UserId -> PhoneUpdate -> Handler NoContent
changePhone uid upd = NoContent <$ Intra.changePhone uid upd

deleteUser :: UserId -> Maybe Email -> Maybe Phone -> Handler NoContent
deleteUser uid mbEmail mbPhone = do
  emailOrPhone <- doubleMaybeToEither "email, phone" mbEmail mbPhone
  usrs <- Intra.getUserProfilesByIdentity emailOrPhone
  case usrs of
    [accountUser -> u] ->
      if userId u == uid
        then do
          info $ userMsg uid . msg (val "Deleting account")
          void $ Intra.deleteAccount uid
          pure NoContent
        else throwE $ mkError status400 "match-error" "email or phone did not match UserId"
    (_ : _ : _) -> error "impossible"
    _ -> throwE $ mkError status404 "not-found" "not found"

setTeamStatusH :: Team.TeamStatus -> TeamId -> Handler NoContent
setTeamStatusH status tid = NoContent <$ Intra.setStatusBindingTeam tid status

deleteTeam :: TeamId -> Maybe Bool -> Maybe Email -> Handler NoContent
deleteTeam givenTid (fromMaybe False -> False) (Just email) = do
  acc <- Intra.getUserProfilesByIdentity (Left email) >>= handleNoUser . listToMaybe
  userTid <- (Intra.getUserBindingTeam . userId . accountUser $ acc) >>= handleNoTeam
  when (givenTid /= userTid) $
    throwE bindingTeamMismatch
  tInfo <- Intra.getTeamInfo givenTid
  unless (length (tiMembers tInfo) == 1) $
    throwE wrongMemberCount
  NoContent <$ Intra.deleteBindingTeam givenTid
  where
    handleNoUser = ifNothing (mkError status404 "no-user" "No such user with that email")
    handleNoTeam = ifNothing (mkError status404 "no-binding-team" "No such binding team")
    wrongMemberCount = mkError status403 "wrong-member-count" "Only teams with 1 user can be deleted"
    bindingTeamMismatch = mkError status404 "binding-team-mismatch" "Binding team mismatch"
deleteTeam tid (fromMaybe False -> True) _ = do
  void $ Intra.getTeamData tid -- throws 404 if team does not exist
  NoContent <$ Intra.deleteBindingTeamForce tid
deleteTeam _ _ _ =
  throwE $ mkError status400 "Bad Request" "either email or 'force=true' parameter is required"

isUserKeyBlacklisted :: Maybe Email -> Maybe Phone -> Handler NoContent
isUserKeyBlacklisted mbemail mbphone = do
  emailOrPhone <- doubleMaybeToEither "email, phone" mbemail mbphone
  bl <- Intra.isBlacklisted emailOrPhone
  if bl
    then throwE $ mkError status200 "blacklisted" "The given user key IS blacklisted"
    else throwE $ mkError status404 "not-blacklisted" "The given user key is NOT blacklisted"

addBlacklist :: Maybe Email -> Maybe Phone -> Handler NoContent
addBlacklist mbemail mbphone = do
  emailOrPhone <- doubleMaybeToEither "email, phone" mbemail mbphone
  NoContent <$ Intra.setBlacklistStatus True emailOrPhone

deleteFromBlacklist :: Maybe Email -> Maybe Phone -> Handler NoContent
deleteFromBlacklist mbemail mbphone = do
  emailOrPhone <- doubleMaybeToEither "email, phone" mbemail mbphone
  NoContent <$ Intra.setBlacklistStatus False emailOrPhone

getTeamInfoByMemberEmail :: Email -> Handler TeamInfo
getTeamInfoByMemberEmail e = do
  acc <- Intra.getUserProfilesByIdentity (Left e) >>= handleUser . listToMaybe
  tid <- (Intra.getUserBindingTeam . userId . accountUser $ acc) >>= handleTeam
  Intra.getTeamInfo tid
  where
    handleUser = ifNothing (mkError status404 "no-user" "No such user with that email")
    handleTeam = ifNothing (mkError status404 "no-binding-team" "No such binding team")

getTeamInfo :: TeamId -> Handler TeamInfo
getTeamInfo = Intra.getTeamInfo

getTeamAdminInfo :: TeamId -> Handler TeamAdminInfo
getTeamAdminInfo = fmap toAdminInfo . Intra.getTeamInfo

mkFeatureGetRoute ::
  forall cfg.
  ( IsFeatureConfig cfg,
    S.ToSchema cfg,
    KnownSymbol (FeatureSymbol cfg),
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  ) =>
  TeamId ->
  Handler (WithStatus cfg)
mkFeatureGetRoute = Intra.getTeamFeatureFlag @cfg

type MkFeaturePutConstraints cfg =
  ( IsFeatureConfig cfg,
    FeatureTrivialConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    S.ToSchema cfg,
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  )

mkFeaturePutRouteTrivialConfigNoTTL ::
  forall cfg. (MkFeaturePutConstraints cfg) => TeamId -> FeatureStatus -> Handler NoContent
mkFeaturePutRouteTrivialConfigNoTTL tid status = mkFeaturePutRouteTrivialConfig @cfg tid status Nothing

mkFeaturePutRouteTrivialConfigWithTTL ::
  forall cfg. (MkFeaturePutConstraints cfg) => TeamId -> FeatureStatus -> FeatureTTLDays -> Handler NoContent
mkFeaturePutRouteTrivialConfigWithTTL tid status = mkFeaturePutRouteTrivialConfig @cfg tid status . Just

mkFeaturePutRouteTrivialConfig ::
  forall cfg. (MkFeaturePutConstraints cfg) => TeamId -> FeatureStatus -> Maybe FeatureTTLDays -> Handler NoContent
mkFeaturePutRouteTrivialConfig tid status (maybe FeatureTTLUnlimited convertFeatureTTLDaysToSeconds -> ttl) = do
  let fullStatus = WithStatusNoLock status trivialConfig ttl
  NoContent <$ Intra.setTeamFeatureFlag @cfg tid fullStatus

{-
setSearchVisibility :: JSON ::: TeamId ::: JsonRequest TeamSearchVisibility -> Handler Response
setSearchVisibility (_ ::: tid ::: req) = do
  status :: TeamSearchVisibility <- parseBody req !>> mkError status400 "client-error"
  json <$> Intra.setSearchVisibility tid status

getTeamBillingInfo :: TeamId -> Handler Response
getTeamBillingInfo tid = do
  ti <- Intra.getTeamBillingInfo tid
  case ti of
    Just t -> pure $ json t
    Nothing -> throwE (mkError status404 "no-team" "No team or no billing info for team")

updateTeamBillingInfo :: JSON ::: TeamId ::: JsonRequest TeamBillingInfoUpdate -> Handler Response
updateTeamBillingInfo (_ ::: tid ::: req) = do
  update <- parseBody req !>> mkError status400 "client-error"
  current <- Intra.getTeamBillingInfo tid >>= handleNoTeam
  let changes = parse update current
  Intra.setTeamBillingInfo tid changes
  json <$> Intra.getTeamBillingInfo tid
  where
    handleNoTeam = ifNothing (mkError status404 "no-team" "No team or no billing info for team")
    parse :: TeamBillingInfoUpdate -> TeamBillingInfo -> TeamBillingInfo
    parse TeamBillingInfoUpdate {..} tbi =
      tbi
        { tbiFirstname = maybe (tbiFirstname tbi) fromRange tbiuFirstname,
          tbiLastname = maybe (tbiLastname tbi) fromRange tbiuLastname,
          tbiStreet = maybe (tbiStreet tbi) fromRange tbiuStreet,
          tbiZip = maybe (tbiZip tbi) fromRange tbiuZip,
          tbiCity = maybe (tbiCity tbi) fromRange tbiuCity,
          tbiCountry = maybe (tbiCountry tbi) fromRange tbiuCountry,
          tbiCompany = fromRange <$> tbiuCompany <|> tbiCompany tbi,
          tbiState = fromRange <$> tbiuState <|> tbiState tbi
        }

setTeamBillingInfo :: JSON ::: TeamId ::: JsonRequest TeamBillingInfo -> Handler Response
setTeamBillingInfo (_ ::: tid ::: req) = do
  billingInfo <- parseBody req !>> mkError status400 "client-error"
  current <- Intra.getTeamBillingInfo tid
  when (isJust current) $
    throwE (mkError status403 "existing-team" "Cannot set info on existing team, use update instead")
  Intra.setTeamBillingInfo tid billingInfo
  getTeamBillingInfo tid

getTeamInvoice :: TeamId ::: InvoiceId ::: JSON -> Handler Response
getTeamInvoice (tid ::: iid ::: _) = do
  url <- Intra.getInvoiceUrl tid iid
  pure $ plain (fromStrict url)

getConsentLog :: Email -> Handler Response
getConsentLog e = do
  acc <- listToMaybe <$> Intra.getUserProfilesByIdentity (Left e)
  when (isJust acc) $
    throwE $
      mkError status403 "user-exists" "Trying to access consent log of existing user!"
  consentLog <- Intra.getEmailConsentLog e
  marketo <- Intra.getMarketoResult e
  pure . json $
    object
      [ "consent_log" .= consentLog,
        "marketo" .= marketo
      ]

-}

_getUserData :: UserId -> Handler Response
_getUserData uid = do
  account <- Intra.getUserProfiles (Left [uid]) >>= noSuchUser . listToMaybe
  conns <- Intra.getUserConnections uid
  convs <- Intra.getUserConversations uid
  clts <- Intra.getUserClients uid
  notfs <- Intra.getUserNotifications uid
  consent <- Intra.getUserConsentValue uid
  consentLog <- Intra.getUserConsentLog uid
  cookies <- Intra.getUserCookies uid
  properties <- Intra.getUserProperties uid
  -- Get all info from Marketo too
  let em = userEmail $ accountUser account
  marketo <- maybe (pure noEmail) Intra.getMarketoResult em
  pure . json $
    object
      [ "account" .= account,
        "cookies" .= cookies,
        "connections" .= conns,
        "conversations" .= convs,
        "clients" .= clts,
        "notifications" .= notfs,
        "consent" .= consent,
        "consent_log" .= consentLog,
        "marketo" .= marketo,
        "properties" .= properties
      ]
  where
    noEmail = MarketoResult $ KeyMap.singleton "results" emptyArray

-- Utilities

instance FromByteString a => Servant.FromHttpApiData [a] where
  parseUrlPiece = maybe (Left "not a list of a's") (Right . fromList) . fromByteString' . cs

groupByStatus :: [UserConnection] -> UserConnectionGroups
groupByStatus conns =
  UserConnectionGroups
    { ucgAccepted = byStatus Accepted conns,
      ucgSent = byStatus Sent conns,
      ucgPending = byStatus Pending conns,
      ucgBlocked = byStatus Blocked conns,
      ucgIgnored = byStatus Ignored conns,
      ucgMissingLegalholdConsent = byStatus MissingLegalholdConsent conns,
      ucgTotal = length conns
    }
  where
    byStatus :: Relation -> [UserConnection] -> Int
    byStatus s = length . filter ((==) s . ucStatus)

ifNothing :: Error -> Maybe a -> Handler a
ifNothing e = maybe (throwE e) pure

noSuchUser :: Maybe a -> Handler a
noSuchUser = ifNothing (mkError status404 "no-user" "No such user")
