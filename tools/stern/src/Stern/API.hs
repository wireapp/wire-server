{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Control.Monad.Except
import Data.Aeson hiding (Error, json)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (emptyArray)
import Data.ByteString.Conversion
import Data.Handle (Handle)
import Data.Id
import Data.Proxy (Proxy (..))
import Data.Range
import Data.Schema hiding ((.=))
import Data.String.Conversions (cs)
import Data.Text (unpack)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol)
import qualified Galley.Types.Teams.Intra as Team
import Imports hiding (head)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Utilities
import qualified Network.Wai.Utilities.Server as Server
import Servant (NoContent (NoContent), ServerT, (:<|>) (..))
import qualified Servant
import qualified Servant.Server
import Stern.API.Routes
import Stern.App
import qualified Stern.Intra as Intra
import Stern.Options
import Stern.Types
import System.Logger.Class hiding (Error, name, trace, (.=))
import Util.Options
import Wire.API.Connection
import Wire.API.Routes.Internal.Brig.Connection (ConnectionStatus)
import qualified Wire.API.Routes.Internal.Brig.EJPD as EJPD
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Team.Feature hiding (setStatus)
import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Search

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
                 :<|> RedirectToSwaggerDocsAPI
             )
        )
        ( swaggerDocs
            :<|> sitemapInternal
            :<|> sitemap e
            :<|> sitemapRedirectToSwaggerDocs
        )

-------------------------------------------------------------------------------
-- servant API

sitemap :: Stern.App.Env -> Servant.Server SternAPI
sitemap env = Servant.Server.hoistServer (Proxy @SternAPI) nt sitemap'
  where
    nt :: forall x. Stern.App.Handler x -> Servant.Server.Handler x
    nt m = Servant.Server.Handler . ExceptT $ do
      fmapL renderError <$> Stern.App.runAppT env (runExceptT m)

    renderError :: Error -> Servant.Server.ServerError
    renderError (Error code label message _) =
      Servant.Server.ServerError (statusCode code) (cs label) (cs message) [("Content-type", "application/json")]

sitemap' :: ServerT SternAPI Handler
sitemap' =
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
    :<|> Named @"get-route-applock-config" (mkFeatureGetRoute @AppLockConfig)
    :<|> Named @"put-route-applock-config" (mkFeaturePutRoute @AppLockConfig)
    :<|> Named @"get-route-mls-config" (mkFeatureGetRoute @MLSConfig)
    :<|> Named @"put-route-mls-config" (mkFeaturePutRoute @MLSConfig)
    :<|> Named @"get-search-visibility" getSearchVisibility
    :<|> Named @"put-search-visibility" setSearchVisibility
    :<|> Named @"get-team-invoice" getTeamInvoice
    :<|> Named @"get-team-billing-info" getTeamBillingInfo
    :<|> Named @"put-team-billing-info" updateTeamBillingInfo
    :<|> Named @"post-team-billing-info" setTeamBillingInfo
    :<|> Named @"get-consent-log" getConsentLog
    :<|> Named @"get-user-meta-info" getUserData

sitemapInternal :: Servant.Server SternAPIInternal
sitemapInternal =
  Named @"status" (pure Servant.NoContent)

sitemapRedirectToSwaggerDocs :: Servant.Server RedirectToSwaggerDocsAPI
sitemapRedirectToSwaggerDocs = Named @"swagger-ui-redirect" redirectToSwaggerDocs

-----------------------------------------------------------------------------
-- Handlers

redirectToSwaggerDocs :: Servant.Server.Handler a
redirectToSwaggerDocs = throwError Servant.err301 {Servant.errHeaders = [("Location", "/swagger-ui/index.html")]}

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
  (fromMaybe (unsafeRange 10) . checked @1 @100 @Int32 . fromMaybe 10 -> s) =
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
    ToSchema cfg,
    KnownSymbol (FeatureSymbol cfg),
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  ) =>
  TeamId ->
  Handler (WithStatus cfg)
mkFeatureGetRoute = Intra.getTeamFeatureFlag @cfg

mkFeaturePutRoute ::
  forall cfg.
  ( IsFeatureConfig cfg,
    ToSchema cfg,
    KnownSymbol (FeatureSymbol cfg),
    FromJSON (WithStatusNoLock cfg),
    ToJSON (WithStatusNoLock cfg),
    Typeable cfg
  ) =>
  TeamId ->
  WithStatusNoLock cfg ->
  Handler NoContent
mkFeaturePutRoute tid payload = NoContent <$ Intra.setTeamFeatureFlag @cfg tid payload

type MkFeaturePutConstraints cfg =
  ( IsFeatureConfig cfg,
    FeatureTrivialConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
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

getSearchVisibility :: TeamId -> Handler TeamSearchVisibilityView
getSearchVisibility = Intra.getSearchVisibility

setSearchVisibility :: TeamId -> TeamSearchVisibility -> Handler NoContent
setSearchVisibility tid status = NoContent <$ Intra.setSearchVisibility tid status

getTeamInvoice :: TeamId -> InvoiceId -> Handler Text
getTeamInvoice tid iid = cs <$> Intra.getInvoiceUrl tid iid

getTeamBillingInfo :: TeamId -> Handler TeamBillingInfo
getTeamBillingInfo tid = do
  let notfound = throwE (mkError status404 "no-team" "No team or no billing info for team")
  Intra.getTeamBillingInfo tid >>= maybe notfound pure

updateTeamBillingInfo :: TeamId -> TeamBillingInfoUpdate -> Handler TeamBillingInfo
updateTeamBillingInfo tid update = do
  current <- Intra.getTeamBillingInfo tid >>= handleNoTeam
  let changes = parse update current
  Intra.setTeamBillingInfo tid changes
  Intra.getTeamBillingInfo tid >>= handleNoTeam
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

setTeamBillingInfo :: TeamId -> TeamBillingInfo -> Handler TeamBillingInfo
setTeamBillingInfo tid billingInfo = do
  current <- Intra.getTeamBillingInfo tid
  when (isJust current) $
    throwE (mkError status403 "existing-team" "Cannot set info on existing team, use update instead")
  Intra.setTeamBillingInfo tid billingInfo
  getTeamBillingInfo tid

getConsentLog :: Email -> Handler ConsentLogAndMarketo
getConsentLog e = do
  acc <- listToMaybe <$> Intra.getUserProfilesByIdentity (Left e)
  when (isJust acc) $
    throwE $
      mkError status403 "user-exists" "Trying to access consent log of existing user!"
  ConsentLogAndMarketo
    <$> Intra.getEmailConsentLog e
    <*> Intra.getMarketoResult e

getUserData :: UserId -> Handler UserMetaInfo
getUserData uid = do
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
  marketo <- do
    let noEmail = MarketoResult $ KeyMap.singleton "results" emptyArray
    maybe (pure noEmail) Intra.getMarketoResult em
  pure . UserMetaInfo . KeyMap.fromList $
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
