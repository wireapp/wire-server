{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

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

-- | This module contains several categories of SCIM-related types:
--
-- * Extensions for @hscim@ types (like 'ScimUserExtra').
-- * Our wrappers over @hscim@ types (like 'ValidScimUser').
-- * Servant-based API types.
-- * Request and response types for SCIM-related endpoints.
module Wire.API.User.Scim where

import Control.Lens (Prism', makeLenses, mapped, prism', (.~), (?~))
import Control.Monad.Except (throwError)
import Crypto.Hash (hash)
import Crypto.Hash.Algorithms (SHA512)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Attoparsec.ByteString (string)
import Data.Binary.Builder qualified as BB
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..))
import Data.CaseInsensitive qualified as CI
import Data.Code as Code
import Data.Handle (Handle)
import Data.Id (ScimTokenId, TeamId, UserId)
import Data.Json.Util ((#))
import Data.Map qualified as Map
import Data.Misc (PlainTextPassword6)
import Data.Proxy
import Data.Singletons.TH
import Data.Swagger hiding (Operation)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Imports
import SAML2.WebSSO qualified as SAML
import SAML2.WebSSO.Test.Arbitrary ()
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Test.QuickCheck qualified as QC
import Web.HttpApiData (parseHeaderWithPrefix)
import Web.Scim.AttrName (AttrName (..))
import Web.Scim.Class.Auth qualified as Scim.Auth
import Web.Scim.Class.Group qualified as Scim.Group
import Web.Scim.Class.User qualified as Scim.User
import Web.Scim.Filter (AttrPath (..))
import Web.Scim.Schema.Common qualified as Scim
import Web.Scim.Schema.Error qualified as Scim
import Web.Scim.Schema.PatchOp (Operation (..), Path (NormalPath))
import Web.Scim.Schema.PatchOp qualified as Scim
import Web.Scim.Schema.Schema (Schema (CustomSchema))
import Web.Scim.Schema.Schema qualified as Scim
import Web.Scim.Schema.User qualified as Scim
import Web.Scim.Schema.User qualified as Scim.User
import Wire.API.Team.Role (Role)
import Wire.API.User.Identity (Email)
import Wire.API.User.Profile as BT
import Wire.API.User.RichInfo qualified as RI
import Wire.API.User.Saml ()
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

----------------------------------------------------------------------------
-- Schemas

userSchemas :: [Scim.Schema]
userSchemas =
  [ Scim.User20,
    Scim.CustomSchema RI.richInfoAssocListURN,
    Scim.CustomSchema RI.richInfoMapURN
  ]

----------------------------------------------------------------------------
-- Token

-- | > docs/reference/provisioning/scim-token.md {#RefScimToken}
--
-- A bearer token that authorizes a provisioning tool to perform actions with a team. Each
-- token corresponds to one team.
--
-- For SCIM authentication and token handling logic, see "Spar.Scim.Auth".
newtype ScimToken = ScimToken {fromScimToken :: Text}
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromByteString, ToByteString)

newtype ScimTokenHash = ScimTokenHash {fromScimTokenHash :: Text}
  deriving (Eq, Show)

instance FromByteString ScimTokenHash where
  parser = string "sha512:" *> (ScimTokenHash <$> parser)

instance ToByteString ScimTokenHash where
  builder (ScimTokenHash t) = BB.fromByteString "sha512:" <> builder t

data ScimTokenLookupKey
  = ScimTokenLookupKeyHashed ScimTokenHash
  | ScimTokenLookupKeyPlaintext ScimToken
  deriving (Eq, Show)

hashScimToken :: ScimToken -> ScimTokenHash
hashScimToken token =
  let digest = hash @ByteString @SHA512 (encodeUtf8 (fromScimToken token))
   in ScimTokenHash (cs @ByteString @Text (convertToBase Base64 digest))

-- | Metadata that we store about each token.
data ScimTokenInfo = ScimTokenInfo
  { -- | Which team can be managed with the token
    stiTeam :: !TeamId,
    -- | Token ID, can be used to eg. delete the token
    stiId :: !ScimTokenId,
    -- | Time of token creation
    stiCreatedAt :: !UTCTime,
    -- | IdP that created users will "belong" to
    stiIdP :: !(Maybe SAML.IdPId),
    -- | Free-form token description, can be set
    --   by the token creator as a mental aid
    stiDescr :: !Text
  }
  deriving (Eq, Show)

instance FromHttpApiData ScimToken where
  parseHeader h = ScimToken <$> parseHeaderWithPrefix "Bearer " h
  parseQueryParam p = ScimToken <$> parseQueryParam p

instance ToHttpApiData ScimToken where
  toHeader (ScimToken s) = "Bearer " <> encodeUtf8 s
  toQueryParam (ScimToken s) = toQueryParam s

instance FromJSON ScimTokenInfo where
  parseJSON = A.withObject "ScimTokenInfo" $ \o -> do
    stiTeam <- o A..: "team"
    stiId <- o A..: "id"
    stiCreatedAt <- o A..: "created_at"
    stiIdP <- o A..:? "idp"
    stiDescr <- o A..: "description"
    pure ScimTokenInfo {..}

instance ToJSON ScimTokenInfo where
  toJSON s =
    A.object $
      "team" A..= stiTeam s
        # "id" A..= stiId s
        # "created_at" A..= stiCreatedAt s
        # "idp" A..= stiIdP s
        # "description" A..= stiDescr s
        # []

----------------------------------------------------------------------------
-- @hscim@ extensions and wrappers

data SparTag

instance Scim.User.UserTypes SparTag where
  type UserId SparTag = UserId
  type UserExtra SparTag = ScimUserExtra
  supportedSchemas = userSchemas

instance Scim.Group.GroupTypes SparTag where
  type GroupId SparTag = ()

instance Scim.Auth.AuthTypes SparTag where
  type AuthData SparTag = ScimToken
  type AuthInfo SparTag = ScimTokenInfo

-- | Wrapper to work around complications with type synonym family application in instances.
--
-- Background: 'SparTag' is used to instantiate the open type families in the classes
-- @Scim.UserTypes@, @Scim.GroupTypes@, @Scim.AuthTypes@.  Those type families are not
-- injective, and in general they shouldn't be: it should be possible to map two tags to
-- different user ids, but the same extra user info.  This makes the type of the 'Cql'
-- instance for @'Scim.StoredUser' tag@ undecidable: if the type checker encounters a
-- constraint that gives it the user id and extra info, it can't compute the tag from that to
-- look up the instance.
--
-- Possible solutions:
--
-- * what we're doing here: wrap the type synonyms we can't instantiate into newtypes in the
--   code using hscim.
--
-- * do not instantiate the type synonym, but its value (in this case
--   @Web.Scim.Schema.Meta.WithMeta (Web.Scim.Schema.Common.WithId (Id U) (Scim.User tag))@
--
-- * Use newtypes instead type in hscim.  This will carry around the tag as a data type rather
--   than applying it, which in turn will enable ghc to type-check instances like @Cql
--   (Scim.StoredUser tag)@.
--
-- * make the type classes parametric in not only the tag, but also all the values of the type
--   families, and add functional dependencies, like this: @class UserInfo tag uid extrainfo |
--   (uid, extrainfo) -> tag, tag -> (uid, extrainfo)@.  this will make writing the instances
--   only a little more awkward, but the rest of the code should change very little, as long
--   as we just apply the type families rather than explicitly imposing the class constraints.
--
-- * given a lot of time: extend ghc with something vaguely similar to @AllowAmbigiousTypes@,
--   where the instance typechecks, and non-injectivity errors are raised when checking the
--   constraint that "calls" the instance.  :)
newtype WrappedScimStoredUser tag = WrappedScimStoredUser
  {fromWrappedScimStoredUser :: Scim.User.StoredUser tag}

-- | See 'WrappedScimStoredUser'.
newtype WrappedScimUser tag = WrappedScimUser
  {fromWrappedScimUser :: Scim.User.User tag}

-- | Extra Wire-specific data contained in a SCIM user profile.
data ScimUserExtra = ScimUserExtra
  { _sueRichInfo :: RI.RichInfo
  }
  deriving (Eq, Show)

makeLenses ''ScimUserExtra

instance A.FromJSON ScimUserExtra where
  parseJSON v = ScimUserExtra <$> A.parseJSON v

instance A.ToJSON ScimUserExtra where
  toJSON (ScimUserExtra rif) = A.toJSON rif

instance QC.Arbitrary ScimUserExtra where
  arbitrary = ScimUserExtra <$> QC.arbitrary

instance QC.Arbitrary (Scim.User SparTag) where
  arbitrary =
    addFields =<< (Scim.empty <$> genSchemas <*> genUserName <*> genExtra)
    where
      addFields :: Scim.User.User tag -> QC.Gen (Scim.User.User tag)
      addFields usr = do
        gexternalId <- cs . QC.getPrintableString <$$> QC.arbitrary
        gdisplayName <- cs . QC.getPrintableString <$$> QC.arbitrary
        gactive <- Just . Scim.ScimBool <$> QC.arbitrary -- (`Nothing` maps on `Just True` and was in the way of a unit test.)
        gemails <- catMaybes <$> (A.decode <$$> QC.listOf (QC.elements ["a@b.c", "x@y,z", "roland@st.uv"]))
        pure
          usr
            { Scim.User.externalId = gexternalId,
              Scim.User.displayName = gdisplayName,
              Scim.User.active = gactive,
              Scim.User.emails = gemails
            }

      genSchemas :: QC.Gen [Scim.Schema]
      genSchemas = QC.listOf1 $ QC.elements Scim.fakeEnumSchema

      genUserName :: QC.Gen Text
      genUserName = cs . QC.getPrintableString <$> QC.arbitrary

      genExtra :: QC.Gen ScimUserExtra
      genExtra = QC.arbitrary

instance Scim.Patchable ScimUserExtra where
  applyOperation (ScimUserExtra (RI.RichInfo rinfRaw)) (Operation o (Just (NormalPath (AttrPath (Just (CustomSchema sch)) (AttrName (CI.mk -> ciAttrName)) Nothing))) val)
    | sch == RI.richInfoMapURN =
        let rinf = RI.richInfoMap $ RI.fromRichInfoAssocList rinfRaw
            unrinf = ScimUserExtra . RI.RichInfo . RI.toRichInfoAssocList . RI.mkRichInfoMapAndList . fmap (uncurry RI.RichField) . Map.assocs
         in unrinf <$> case o of
              Scim.Remove ->
                pure $ Map.delete ciAttrName rinf
              _AddOrReplace ->
                case val of
                  (Just (A.String textVal)) ->
                    pure $ Map.insert ciAttrName textVal rinf
                  _ -> throwError $ Scim.badRequest Scim.InvalidValue $ Just "rich info values can only be text"
    | sch == RI.richInfoAssocListURN =
        let rinf = RI.richInfoAssocList $ RI.fromRichInfoAssocList rinfRaw
            unrinf = ScimUserExtra . RI.RichInfo . RI.toRichInfoAssocList . RI.mkRichInfoMapAndList
            matchesAttrName (RI.RichField k _) = k == ciAttrName
         in unrinf <$> case o of
              Scim.Remove ->
                pure $ filter (not . matchesAttrName) rinf
              _AddOrReplace ->
                case val of
                  (Just (A.String textVal)) ->
                    let newField = RI.RichField ciAttrName textVal
                        replaceIfMatchesAttrName f = if matchesAttrName f then newField else f
                        newRichInfo =
                          if not $ any matchesAttrName rinf
                            then rinf ++ [newField]
                            else map replaceIfMatchesAttrName rinf
                     in pure newRichInfo
                  _ -> throwError $ Scim.badRequest Scim.InvalidValue $ Just "rich info values can only be text"
    | otherwise = throwError $ Scim.badRequest Scim.InvalidValue $ Just "unknown schema, cannot patch"
  applyOperation _ _ = throwError $ Scim.badRequest Scim.InvalidValue $ Just "invalid patch op for rich info"

-- | SCIM user with all the data spar is actively processing.  Constructed by
-- 'validateScimUser', or manually from data obtained from brig to pass them on to scim peers.
-- The idea is that the type we get back from hscim is too general, and
-- we need a second round of parsing (aka validation), of which 'ValidScimUser' is the result.
--
-- Data contained in '_vsuHandle' and '_vsuName' is guaranteed to a) correspond to the data in
-- the 'Scim.User.User' and b) be valid in regard to our own user schema requirements (only
-- certain characters allowed in handles, etc).
--
-- Note that it's ok for us to ignore parts of the content sent to us, as explained
-- [here](https://tools.ietf.org/html/rfc7644#section-3.3): "Since the server is free to alter
-- and/or ignore POSTed content, returning the full representation can be useful to the
-- client, enabling it to correlate the client's and server's views of the new resource."
data ValidScimUser = ValidScimUser
  { _vsuExternalId :: ValidExternalId,
    _vsuHandle :: Handle,
    _vsuName :: BT.Name,
    _vsuRichInfo :: RI.RichInfo,
    _vsuActive :: Bool,
    _vsuLocale :: Maybe Locale,
    _vsuRole :: Maybe Role
  }
  deriving (Eq, Show)

-- | Note that a 'SAML.UserRef' may contain an email. Even though it is possible to construct a 'ValidExternalId' from such a 'UserRef' with 'UrefOnly',
-- this does not represent a valid 'ValidExternalId'. So in case of a 'UrefOnly', we can assume that the 'UserRef' does not contain an email.
data ValidExternalId
  = EmailAndUref Email SAML.UserRef
  | UrefOnly SAML.UserRef
  | EmailOnly Email
  deriving (Eq, Show, Generic)

-- | Take apart a 'ValidExternalId', using 'SAML.UserRef' if available, otherwise 'Email'.
runValidExternalIdEither :: (SAML.UserRef -> a) -> (Email -> a) -> ValidExternalId -> a
runValidExternalIdEither doUref doEmail = \case
  EmailAndUref _ uref -> doUref uref
  UrefOnly uref -> doUref uref
  EmailOnly em -> doEmail em

-- | Take apart a 'ValidExternalId', use both 'SAML.UserRef', 'Email' if applicable, and
-- merge the result with a given function.
runValidExternalIdBoth :: (a -> a -> a) -> (SAML.UserRef -> a) -> (Email -> a) -> ValidExternalId -> a
runValidExternalIdBoth merge doUref doEmail = \case
  EmailAndUref eml uref -> doUref uref `merge` doEmail eml
  UrefOnly uref -> doUref uref
  EmailOnly em -> doEmail em

veidUref :: Prism' ValidExternalId SAML.UserRef
veidUref = prism' UrefOnly $
  \case
    EmailAndUref _ uref -> Just uref
    UrefOnly uref -> Just uref
    EmailOnly _ -> Nothing

veidEmail :: Prism' ValidExternalId Email
veidEmail = prism' EmailOnly $
  \case
    EmailAndUref em _ -> Just em
    UrefOnly _ -> Nothing
    EmailOnly em -> Just em

makeLenses ''ValidScimUser
makeLenses ''ValidExternalId

----------------------------------------------------------------------------
-- Request and response types

-- | Type used for request parameters to 'APIScimTokenCreate'.
data CreateScimToken = CreateScimToken
  { -- | Token description (as memory aid for whoever is creating the token)
    createScimTokenDescr :: !Text,
    -- | User password, which we ask for because creating a token is a "powerful" operation
    createScimTokenPassword :: !(Maybe PlainTextPassword6),
    -- | User code (sent by email), for 2nd factor to 'createScimTokenPassword'
    createScimTokenCode :: !(Maybe Code.Value)
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CreateScimToken)

instance A.FromJSON CreateScimToken where
  parseJSON = A.withObject "CreateScimToken" $ \o -> do
    createScimTokenDescr <- o A..: "description"
    createScimTokenPassword <- o A..:? "password"
    createScimTokenCode <- o A..:? "verification_code"
    pure CreateScimToken {..}

-- Used for integration tests
instance A.ToJSON CreateScimToken where
  toJSON CreateScimToken {..} =
    A.object
      [ "description" A..= createScimTokenDescr,
        "password" A..= createScimTokenPassword,
        "verification_code" A..= createScimTokenCode
      ]

-- | Type used for the response of 'APIScimTokenCreate'.
data CreateScimTokenResponse = CreateScimTokenResponse
  { createScimTokenResponseToken :: ScimToken,
    createScimTokenResponseInfo :: ScimTokenInfo
  }
  deriving (Eq, Show)

-- Used for integration tests
instance A.FromJSON CreateScimTokenResponse where
  parseJSON = A.withObject "CreateScimTokenResponse" $ \o -> do
    createScimTokenResponseToken <- o A..: "token"
    createScimTokenResponseInfo <- o A..: "info"
    pure CreateScimTokenResponse {..}

instance A.ToJSON CreateScimTokenResponse where
  toJSON CreateScimTokenResponse {..} =
    A.object
      [ "token" A..= createScimTokenResponseToken,
        "info" A..= createScimTokenResponseInfo
      ]

-- | Type used for responses of endpoints that return a list of SCIM tokens.
-- Wrapped into an object to allow extensibility later on.
--
-- We don't show tokens once they have been created – only their metadata.
data ScimTokenList = ScimTokenList
  { scimTokenListTokens :: [ScimTokenInfo]
  }
  deriving (Eq, Show)

instance A.FromJSON ScimTokenList where
  parseJSON = A.withObject "ScimTokenList" $ \o -> do
    scimTokenListTokens <- o A..: "tokens"
    pure ScimTokenList {..}

instance A.ToJSON ScimTokenList where
  toJSON ScimTokenList {..} =
    A.object
      [ "tokens" A..= scimTokenListTokens
      ]

-- Swagger

instance ToParamSchema ScimToken where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema ScimToken where
  declareNamedSchema _ =
    declareNamedSchema (Proxy @Text)
      & mapped . schema . description ?~ "Authentication token"

instance ToSchema ScimTokenInfo where
  declareNamedSchema _ = do
    teamSchema <- declareSchemaRef (Proxy @TeamId)
    idSchema <- declareSchemaRef (Proxy @ScimTokenId)
    createdAtSchema <- declareSchemaRef (Proxy @UTCTime)
    idpSchema <- declareSchemaRef (Proxy @SAML.IdPId)
    descrSchema <- declareSchemaRef (Proxy @Text)
    pure $
      NamedSchema (Just "ScimTokenInfo") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ [ ("team", teamSchema),
                 ("id", idSchema),
                 ("created_at", createdAtSchema),
                 ("idp", idpSchema),
                 ("description", descrSchema)
               ]
          & required .~ ["team", "id", "created_at", "description"]

instance ToSchema CreateScimToken where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    pure $
      NamedSchema (Just "CreateScimToken") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ [ ("description", textSchema),
                 ("password", textSchema),
                 ("verification_code", textSchema)
               ]
          & required .~ ["description"]

instance ToSchema CreateScimTokenResponse where
  declareNamedSchema _ = do
    tokenSchema <- declareSchemaRef (Proxy @ScimToken)
    infoSchema <- declareSchemaRef (Proxy @ScimTokenInfo)
    pure $
      NamedSchema (Just "CreateScimTokenResponse") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ [ ("token", tokenSchema),
                 ("info", infoSchema)
               ]
          & required .~ ["token", "info"]

instance ToSchema ScimTokenList where
  declareNamedSchema _ = do
    infoListSchema <- declareSchemaRef (Proxy @[ScimTokenInfo])
    pure $
      NamedSchema (Just "ScimTokenList") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ [ ("tokens", infoListSchema)
               ]
          & required .~ ["tokens"]

-- | This is used in `ValidUAuthIdF f` with Const () and Identity functors.
-- Const () means that there is no value in that position, and it can be ignored.
-- Identity means that there is a value in that position, and it needs to be considered.
--
-- NOTE(fisx): we're using dot syntax for records these days, so ambiguous field names are ok!
data UAuthIdF a b c = UAuthIdF
  { samlId :: a SAML.UserRef,
    scimExternalId :: b Text,
    email :: c Email,
    -- | only team users support saml and/or scim.  exzternalId is scoped in
    -- team, so once we have parsed a scim user record, the externalId should
    -- never occur anywhere without team it!
    teamId :: TeamId
  }
  deriving (Generic)

-- | Konst exists to make the next set of declarations easier to write
type Konst = Const ()

-- | We currently support the following user authentication and provisioning setups.
--
-- (there is a sixth case: no scim, no saml, email and password, but that does not involve
-- spar, so we don't need to consider that setup here.)
--
-- (We also sometimes validate email addresses by sending an email with a code from brig, and
-- sometimes not, but i think this should be documented and considered elsewhere.)
data UAuthIdTag
  = -- | scim with saml and email (distinguishable by location: nameid + externalId vs. scim emails field)
    UAScimSamlEmail
  | -- | scim with saml and no email
    UAScimSamlNoEmail
  | -- | scim, email and password (via traditional invitation email, emulating the team-settings flow), no saml
    UAScimEmailNoSaml
  | -- | no scim, saml with email in nameid
    UASamlEmailNoScim
  | -- | no scim, saml with no email
    UASamlNoScimNoEmail
  deriving (Show, Eq, Generic, Bounded, Enum)

$(genSingletons [''UAuthIdTag])
$(singDecideInstance ''UAuthIdTag)

-- | The types we actually want to use.
-- This is using the closed form, giving us a single place to define
-- what is and is not a valid combination of functors in `UAuthIdF`
--
-- NOTE(fisx): this may not be as useful as we'd hope, but `UAuthIdF` may be more useful in
-- return.  example: a function that cares about email, but not about samlId or
-- scimExternalId, should be typed using `UAuthIdF a b Konst`.  But there are probably a few
-- places where we want to limit the number of legal combinations, especially when parsing
-- incoming data.
{- ORMOLU_DISABLE -}
type family ValidUAuthIdF (f :: UAuthIdTag) where
  ValidUAuthIdF 'UAScimSamlEmail     = UAuthIdF Identity Identity Identity
  ValidUAuthIdF 'UAScimSamlNoEmail   = UAuthIdF Identity Identity Konst
  ValidUAuthIdF 'UAScimEmailNoSaml   = UAuthIdF Konst    Identity Identity
  ValidUAuthIdF 'UASamlEmailNoScim   = UAuthIdF Identity Konst    Identity
  ValidUAuthIdF 'UASamlNoScimNoEmail = UAuthIdF Identity Konst    Konst
{- ORMOLU_ENABLE -}

-- | In brig, we don't really care about these values and never have to validate them.  We
-- just get them from spar, and write them to the database and later communicate them back to
-- spar or to team-management or to clients.  So in order to keep things from getting out of
-- hand, we decide the presence of all fields at run time.
type PartialUAuthId = UAuthIdF 'Maybe 'Maybe 'Maybe

runValidUAuthIdFEither ::
  forall tag a.
  SingI tag =>
  (SAML.UserRef -> a) ->
  (Email -> a) ->
  ValidUAuthIdF tag ->
  a
runValidUAuthIdFEither doUref doEmail extId = case tag of
  -- TODO!
  SScimExternalId -> undefined $ runIdentity extId.eScim
  SSaml -> doUref $ runIdentity extId.eSaml
  SSamlAndEmail -> doUref $ runIdentity extId.eSaml
  SSamlAndPassword -> doUref $ runIdentity extId.eSaml
  SEmail -> doEmail $ runIdentity extId.eEmail
  where
    tag = sing @tag

runValidUAuthIdFBoth ::
  forall tag a.
  SingI tag =>
  (a -> a -> a) ->
  (SAML.UserRef -> a) ->
  (Email -> a) ->
  ValidUAuthIdF tag ->
  a
runValidUAuthIdFBoth merge doUref doEmail extId = case tag of
  -- TODO!
  SScim -> undefined $ runIdentity extId.eScim
  SSaml -> doUref $ runIdentity extId.eSaml
  SSamlAndEmail ->
    doUref (runIdentity extId.eSaml)
      `merge` doEmail (runIdentity extId.eEmail)
  SSamlAndPassword -> doUref $ runIdentity extId.eSaml
  SEmail -> doEmail $ runIdentity extId.eEmail
  -- TODO!
  SExternalId -> undefined $ runIdentity extId.eExternalId
  where
    tag = sing @tag
