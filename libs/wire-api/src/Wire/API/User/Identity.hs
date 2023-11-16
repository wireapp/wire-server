{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.User.Identity
  ( -- * UserIdentity
    UserIdentity (..),
    newIdentity,
    emailIdentity,
    phoneIdentity,
    uauthIdentity,
    legacySsoIdentity,
    userIdentityObjectSchema,
    maybeUserIdentityObjectSchema,
    eUserIdentityObjectSchema,
    eUserIdentityFromComponents,
    eUserIdentityToComponents,
    UserIdentityComponents,
    UserIdentityFromComponentsParseErrors (..),
    LegacyUserSSOId (..),

    -- * Email
    Email (..),
    fromEmail,
    parseEmail,
    validateEmail,

    -- * Phone
    Phone (..),
    parsePhone,
    isValidPhone,

    -- * UAuthId
    UAuthId (..),
    EmailWithSource (..),
    EmailSource (..),
    PartialUAuthId,
    ScimUAuthId,
    partialToScimUAuthId,
    scimToPartialUAuthId,

    -- * helpers
    emailFromSAML,
    emailToSAML,
    emailToSAMLNameID,
    emailFromSAMLNameID,
    mkUserNameScim,
    mkUserNameSaml,
    mkSampleUref,
    mkSimpleSampleUref,
    mkBasicSampleUref,
  )
where

import Control.Applicative (optional)
import Control.Lens (dimap, over, view, (.~), (?~), (^.))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Attoparsec.Text
import Data.Bifunctor (first)
import Data.ByteString.Conversion
import Data.CaseInsensitive qualified as CI
import Data.Id
import Data.OpenApi (ToParamSchema (..))
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock
import GHC.TypeLits
import Imports
import SAML2.WebSSO.Test.Arbitrary ()
import SAML2.WebSSO.Types qualified as SAML
import SAML2.WebSSO.Types.Email qualified as SAMLEmail
import SAML2.WebSSO.XML qualified as SAML
import Servant
import Servant.API qualified as S
import System.FilePath ((</>))
import Test.QuickCheck qualified as QC
import Text.Email.Validate qualified as Email.V
import URI.ByteString qualified as URI
import URI.ByteString.QQ (uri)
import Wire.API.User.Profile
import Wire.API.User.Types
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- UserIdentity

-- | The private unique user identity that is used for login and
-- account recovery.
--
-- Explaining the teamFieldName phantom type is a little more involved.
--
-- `UserIdentity` gets parsed from object schemas of different containing types.  `User`,
-- `NewUser` are two good examples: both haskell types contain a field typed `UserIdentity`,
-- but the schema of the inner type is merged with the schema of the outer, ie., the parser
-- collects all fields on the root object.
--
-- Now in `User`, the field carrying the team id is called `team`; in `NewUser`, that name is
-- already used for team name, so `team_id` is used instead.  So we need to parameterize over
-- the name of that field.
data UserIdentity (tf :: Symbol)
  = FullIdentity Email Phone
  | EmailIdentity Email
  | PhoneIdentity Phone
  | UAuthIdentity PartialUAuthId (Maybe Email {- from `brig.user.email`, which may differ from the email address inside UAuthId -})
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform (UserIdentity tf))

-- | The unparsed data as retrieved from cassandra or json.  See source code and
-- `userIdentityComponentsObjectSchema`, `maybeUserIdentityFromComponents`,
-- `maybeUserIdentityToComponents` below.
userIdentityObjectSchema :: forall tf. KnownSymbol tf => ObjectSchema SwaggerDoc (UserIdentity tf)
userIdentityObjectSchema =
  Right .= withParser (eUserIdentityObjectSchema @tf) (either (fail . show) pure)

eUserIdentityObjectSchema :: forall tf. KnownSymbol tf => ObjectSchema SwaggerDoc (Either UserIdentityFromComponentsParseErrors (UserIdentity tf))
eUserIdentityObjectSchema =
  dimap eUserIdentityToComponents eUserIdentityFromComponents (userIdentityComponentsObjectSchema @tf)

maybeUserIdentityObjectSchema :: forall tf. KnownSymbol tf => ObjectSchema SwaggerDoc (Maybe (UserIdentity tf))
maybeUserIdentityObjectSchema =
  dimap (maybeUserIdentityToComponents @tf) (maybeUserIdentityFromComponents @tf) (userIdentityComponentsObjectSchema @tf)

-- | See `UAuthId` docs for an explanation of the phantom type parameter.
type UserIdentityComponents (tf :: Symbol) =
  ( -- email from `brig.user.email`
    Maybe Email,
    -- phone from `brig.user.phone`
    Maybe Phone,
    -- user identifying data from spar
    Maybe PartialUAuthId,
    -- user auth info from spar (legacy)
    Maybe LegacyUserSSOId,
    -- `TeamId`, `ManagedBy` are needed for migration from `LegacyUserSSOId` to
    -- `PartialUAuthId`.  When parsing, eg., `User` json values, these are duplicated from the
    -- fields also in that record.  During rendering, they will be ignored (because we always
    -- have a UAuthId at that point: parsed `UserIdentity` values always contain the migrated
    -- `UAuthId` value).  `UserIdentityComponents` values constructed from `UAuthId` values
    -- will always be 'Nothing'.
    Maybe TeamId,
    Maybe ManagedBy
  )

userIdentityComponentsObjectSchema :: forall tf. KnownSymbol tf => ObjectSchema SwaggerDoc (UserIdentityComponents tf)
userIdentityComponentsObjectSchema =
  (,,,,,)
    <$> (\(a, _, _, _, _, _) -> a) .= maybe_ (optField "email" schema)
    <*> (\(_, a, _, _, _, _) -> a) .= maybe_ (optField "phone" schema)
    <*> (\(_, _, a, _, _, _) -> a)
      .= ( -- FUTUREWORK(fisx): The UAuthId aeson parser succeeds iff team and either sso or
           -- saml id are present.  Since this is not the case for user components leading to
           -- eg. EmailIdentity, we need to recover from failures here.  Bad news is that
           -- there won't be any parse errors even if legitimate, but I can't think of a good
           -- way to solve this.
           maybe_ (optField "uauth_id" genericToSchema) <|> pure Nothing
         )
    <*> (\(_, _, _, a, _, _) -> a) .= maybe_ (optField "sso_id" genericToSchema)
    <*> (\(_, _, _, _, a, _) -> a) .= maybe_ (optField (cs $ symbolVal (Proxy @tf)) genericToSchema)
    <*> (\(_, _, _, _, _, a) -> a) .= maybe_ (optField "managed_by" genericToSchema)

data UserIdentityFromComponentsParseErrors
  = UserIdentityFromComponentsNoFields
  | UserIdentityFromComponentsNoPhoneAllowedForUAuthId
  | UserIdentityFromComponentsUAuthIdWithoutTeam
  | UserIdentityFromComponentsUAuthIdTeamMismatch
  | UserIdentityFromComponentsEmailAndSourceMustComeTogetherForLegacyMigration
  deriving (Eq, Show)

eUserIdentityFromComponents :: UserIdentityComponents tf -> Either UserIdentityFromComponentsParseErrors (UserIdentity tf)
eUserIdentityFromComponents = \case
  -- old-school
  (Just eml, Just phn, Nothing, Nothing, _, _) -> Right $ FullIdentity eml phn
  (Just eml, Nothing, Nothing, Nothing, _, _) -> Right $ EmailIdentity eml
  (Nothing, Just phn, Nothing, Nothing, _, _) -> Right $ PhoneIdentity phn
  --
  -- uauth (from uauth_id; sso_id field will be ignored)
  (_, Just _, Just _, _, _, _) -> Left UserIdentityFromComponentsNoPhoneAllowedForUAuthId
  (_, Just _, Nothing, Just _, _, _) -> Left UserIdentityFromComponentsNoPhoneAllowedForUAuthId
  (mbeml, Nothing, Just uauth, _, mbteamid, _) ->
    -- `mbteamid` must be Nothing or match `uauth.uaTeamId`.  `mbeml` and `uauth.uaEmail` do
    -- not need to match: the email address in the first part of the tuple is the validated
    -- one stored in brig.  eg., the one stored in the `PartialUAuthId` may just have been
    -- uploaded by scim and not yet validated.
    case mbteamid of
      Nothing -> Right $ UAuthIdentity uauth mbeml
      Just teamid ->
        if uauth.uaTeamId == teamid
          then Right $ UAuthIdentity uauth mbeml
          else Left UserIdentityFromComponentsUAuthIdTeamMismatch
  --
  -- uauth (legacy; uauth_id is missing and sso_id is considered instead)
  (mbemail, Nothing, Nothing, Just lsso, Just teamid, fromMaybe ManagedByWire -> mby) ->
    Right $ UAuthIdentity (legacyUserSSOIdToUAuthId lsso teamid mby mbemail) mbemail
  (_, Nothing, Nothing, Just _, Nothing, _) ->
    Left UserIdentityFromComponentsUAuthIdWithoutTeam
  --
  -- catchall
  (Nothing, Nothing, Nothing, Nothing, _, _) ->
    Left UserIdentityFromComponentsNoFields

-- | Wrapper for `eUserIdentityFromComponents`.
maybeUserIdentityFromComponents :: forall tf. UserIdentityComponents tf -> Maybe (UserIdentity tf)
maybeUserIdentityFromComponents = either (const Nothing) Just . (eUserIdentityFromComponents @tf)

-- | Convert `UserIdentity` back into raw data for json.  The `LegacySSOIdentity` part of
-- the tuple is always `Nothing`.
eUserIdentityToComponents :: forall tf e. Either e (UserIdentity tf) -> UserIdentityComponents tf
eUserIdentityToComponents (Left _) = (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
eUserIdentityToComponents (Right (FullIdentity email phone)) = (Just email, Just phone, Nothing, Nothing, Nothing, Nothing)
eUserIdentityToComponents (Right (EmailIdentity email)) = (Just email, Nothing, Nothing, Nothing, Nothing, Nothing)
eUserIdentityToComponents (Right (PhoneIdentity phone)) = (Nothing, Just phone, Nothing, Nothing, Nothing, Nothing)
eUserIdentityToComponents (Right (UAuthIdentity uaid mbemail)) = (mbemail, Nothing, Just uaid, uAuthIdToLegacyUserSSOId uaid, Nothing, Nothing)

-- | Wrapper for `eUserIdentityToComponents`.
maybeUserIdentityToComponents :: Maybe (UserIdentity tf) -> UserIdentityComponents tf
maybeUserIdentityToComponents = eUserIdentityToComponents . maybe (Left ()) Right

newIdentity :: Maybe Email -> Maybe Phone -> Maybe PartialUAuthId -> Maybe (UserIdentity tf)
newIdentity mbEmail mbPhone mbUAuth = maybeUserIdentityFromComponents (mbEmail, mbPhone, mbUAuth, Nothing, Nothing, Nothing)

emailIdentity :: UserIdentity tf -> Maybe Email
emailIdentity (FullIdentity email _) = Just email
emailIdentity (EmailIdentity email) = Just email
emailIdentity (PhoneIdentity _) = Nothing
emailIdentity (UAuthIdentity _ mbemail) = mbemail -- if uaid.uaEmail diverges, mbemail is not a (validated) part of uaid yet.

phoneIdentity :: UserIdentity tf -> Maybe Phone
phoneIdentity (FullIdentity _ phone) = Just phone
phoneIdentity (PhoneIdentity phone) = Just phone
phoneIdentity (EmailIdentity _) = Nothing
phoneIdentity (UAuthIdentity _ _) = Nothing

uauthIdentity :: UserIdentity tf -> Maybe PartialUAuthId
uauthIdentity (UAuthIdentity uaid _) = pure uaid
uauthIdentity _ = Nothing

legacySsoIdentity :: UserIdentity tf -> Maybe LegacyUserSSOId
legacySsoIdentity (UAuthIdentity uaid _mbemail) = uAuthIdToLegacyUserSSOId uaid
legacySsoIdentity _ = Nothing

-- | FUTUREWORK:
--
-- * Enforce these constrains during parsing already or use a separate type, see
--   [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate).
--
-- * Check for differences to validation of `Data.Domain.Domain` and decide whether to
--   align/de-duplicate the two.
--
-- * Drop dependency on email-validate? We do our own email domain validation anyways,
--   is the dependency worth it just for validating the local part?
validateEmail :: Email -> Either String Email
validateEmail =
  pure . uncurry Email
    <=< validateDomain
    <=< validateExternalLib
    <=< validateLength . fromEmail
  where
    validateLength e
      | len <= 100 = Right e
      | otherwise = Left $ "length " <> show len <> " exceeds 100"
      where
        len = Text.length e
    validateExternalLib e = do
      email <- Email.V.validate $ encodeUtf8 e
      l <- first show . decodeUtf8' $ Email.V.localPart email
      d <- first show . decodeUtf8' $ Email.V.domainPart email
      pure (l, d)
    -- cf. https://en.wikipedia.org/wiki/Email_address#Domain
    -- n.b. We do not allow IP address literals, comments or non-ASCII
    --      characters, mostly because SES (and probably many other mail
    --      systems) don't support that (yet?) either.
    validateDomain (l, d) = parseOnly domain d
      where
        domain = (label *> many1 (char '.' *> label) *> endOfInput) $> (l, d)
        label =
          satisfy (inClass "a-zA-Z0-9")
            *> count 61 (optional (satisfy (inClass "-a-zA-Z0-9")))
            *> optional (satisfy (inClass "a-zA-Z0-9"))

--------------------------------------------------------------------------------
-- Phone

newtype Phone = Phone {fromPhone :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Phone)

instance ToParamSchema Phone where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema Phone where
  schema =
    over doc (S.description ?~ "E.164 phone number") $
      fromPhone .= parsedText "PhoneNumber" (maybe (Left "Invalid phone number. Expected E.164 format.") Right . parsePhone)

instance ToByteString Phone where
  builder = builder . fromPhone

instance FromByteString Phone where
  parser = parser >>= maybe (fail "Invalid phone") pure . parsePhone

instance S.FromHttpApiData Phone where
  parseUrlPiece = maybe (Left "Invalid phone") Right . fromByteString . cs

instance S.ToHttpApiData Phone where
  toUrlPiece = cs . toByteString'

instance Arbitrary Phone where
  arbitrary =
    Phone . Text.pack <$> do
      let mkdigits n = replicateM n (QC.elements ['0' .. '9'])
      mini <- mkdigits 8
      maxi <- mkdigits =<< QC.chooseInt (0, 7)
      pure $ '+' : mini <> maxi

-- | Parses a phone number in E.164 format with a mandatory leading '+'.
parsePhone :: Text -> Maybe Phone
parsePhone p
  | isValidPhone p = Just $! Phone p
  | otherwise = Nothing

-- | Checks whether a phone number is valid, i.e. it is in E.164 format
-- with a mandatory leading '+' followed by 10-15 digits.
isValidPhone :: Text -> Bool
isValidPhone = either (const False) (const True) . parseOnly e164
  where
    e164 = char '+' *> count 8 digit *> count 7 (optional digit) *> endOfInput

-- | If the budget for SMS and voice calls for a phone number
-- has been exhausted within a certain time frame, this timeout
-- indicates in seconds when another attempt may be made.
newtype PhoneBudgetTimeout = PhoneBudgetTimeout
  {phoneBudgetTimeout :: NominalDiffTime}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

instance FromJSON PhoneBudgetTimeout where
  parseJSON = A.withObject "PhoneBudgetTimeout" $ \o ->
    PhoneBudgetTimeout <$> o A..: "expires_in"

instance ToJSON PhoneBudgetTimeout where
  toJSON (PhoneBudgetTimeout t) = A.object ["expires_in" A..= t]

--------------------------------------------------------------------------------
-- LegacyUserSSOId (DEPRECATED, formerly UserSSOId)

-- | User's legacy external identity (DEPRECATED).
-- NB: this type is serialized to the full xml encoding of the `SAML.UserRef` components, but
-- deserialiation is more lenient: it also allows for the `Issuer` to be a plain URL (without
-- xml around it), and the `NameID` to be an email address (=> format "email") or an arbitrary
-- text (=> format "unspecified").  This is for backwards compatibility and general
-- robustness.
data LegacyUserSSOId
  = UserSSOId SAML.UserRef
  | UserScimExternalId Text
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegacyUserSSOId)

legacyUserSSOIdToUAuthId :: LegacyUserSSOId -> TeamId -> ManagedBy -> Maybe Email -> PartialUAuthId
{-
some removed code from spar that describes the semantics of LegacyUserSSOId:

data ValidExternalId
  = EmailAndUref Email SAML.UserRef
  | UrefOnly SAML.UserRef
  | EmailOnly Email

veidToUserSSOId :: ValidExternalId -> UserSSOId
veidToUserSSOId = runValidExternalIdEither UserSSOId (UserScimExternalId . fromEmail)

veidFromUserSSOId :: MonadError String m => UserSSOId -> m ValidExternalId
veidFromUserSSOId = \case
  UserSSOId uref ->
    case urefToEmail uref of
      Nothing -> pure $ UrefOnly uref
      Just email -> pure $ EmailAndUref email uref
  UserScimExternalId email ->
    maybe
      (throwError "externalId not an email and no issuer")
      (pure . EmailOnly)
      (parseEmail email)
-}
legacyUserSSOIdToUAuthId ssoid tid mby mbeml = UAuthId uref' eid' eml' tid
  where
    uref' = case ssoid of
      UserSSOId uref -> Just uref
      UserScimExternalId _ -> Nothing

    eid' = case (ssoid, mby) of
      (UserSSOId _, ManagedByWire) -> Nothing
      (UserSSOId uref, ManagedByScim) -> Just . CI.original . SAML.unsafeShowNameID . view SAML.uidSubject $ uref
      (UserScimExternalId e, _) -> Just e

    -- `EmailFromScimEmailsField` is never has not been introduced yet at the time of writing
    -- this code.  So no legacy users should ever exist that need this value: either they have
    -- been created after introduction of `EmailFromScimEmailsField` and don't need migration,
    -- or they have been introduced before and don't need `EmailFromScimEmailsField`.
    eml' = mbeml <&> (`EmailWithSource` emlsrc)
    emlsrc = case mby of
      ManagedByWire -> EmailFromSamlNameId
      ManagedByScim -> EmailFromScimExternalIdField

uAuthIdToLegacyUserSSOId :: PartialUAuthId -> Maybe LegacyUserSSOId
uAuthIdToLegacyUserSSOId (UAuthId mburef mbeid _mbeml _tid) = case mburef of
  Just uref -> Just $ UserSSOId uref
  Nothing -> case mbeid of
    Just eid -> Just $ UserScimExternalId eid
    Nothing -> Nothing

instance S.ToSchema LegacyUserSSOId where
  declareNamedSchema _ = do
    tenantSchema <- S.declareSchemaRef (Proxy @Text) -- 'Issuer'
    subjectSchema <- S.declareSchemaRef (Proxy @Text) -- 'NameID'
    scimSchema <- S.declareSchemaRef (Proxy @Text)
    pure $
      S.NamedSchema (Just "UserSSOId") $
        mempty
          & S.type_ ?~ S.OpenApiObject
          & S.properties
            .~ [ ("tenant", tenantSchema),
                 ("subject", subjectSchema),
                 ("scim_external_id", scimSchema)
               ]

instance ToJSON LegacyUserSSOId where
  toJSON = \case
    UserSSOId (SAML.UserRef tenant subject) ->
      A.object ["tenant" A..= SAML.encodeElem tenant, "subject" A..= SAML.encodeElem subject]
    UserScimExternalId eid ->
      A.object ["scim_external_id" A..= eid]

instance FromJSON LegacyUserSSOId where
  parseJSON = A.withObject "UserSSOId" $ \obj -> do
    mtenant <- lenientlyParseSAMLIssuer =<< (obj A..:? "tenant")
    msubject <- lenientlyParseSAMLNameID =<< (obj A..:? "subject")
    meid <- obj A..:? "scim_external_id"
    case (mtenant, msubject, meid) of
      (Just tenant, Just subject, Nothing) ->
        pure $ UserSSOId (SAML.UserRef tenant subject)
      (Nothing, Nothing, Just eid) ->
        pure $ UserScimExternalId eid
      _ -> fail "either need tenant and subject, or scim_external_id, but not both"

----------------------------------------------------------------------
-- low-level helper functions

lenientlyParseSAMLIssuer :: Maybe LText -> A.Parser (Maybe SAML.Issuer)
lenientlyParseSAMLIssuer mbtxt = forM mbtxt $ \txt -> do
  let asxml :: Either String SAML.Issuer
      asxml = SAML.decodeElem txt

      asurl :: Either String SAML.Issuer
      asurl =
        bimap show SAML.Issuer $
          URI.parseURI URI.laxURIParserOptions (cs txt)

      err :: String
      err = "lenientlyParseSAMLIssuer: " <> show (asxml, asurl, mbtxt)

  either (const $ fail err) pure $ asxml <|> asurl

lenientlyParseSAMLNameID :: Maybe LText -> A.Parser (Maybe SAML.NameID)
lenientlyParseSAMLNameID Nothing = pure Nothing
lenientlyParseSAMLNameID (Just txt) = do
  let asxml :: Either String SAML.NameID
      asxml = SAML.decodeElem txt

      asemail :: Either String SAML.NameID
      asemail =
        maybe
          (Left "not an email")
          (fmap emailToSAMLNameID . validateEmail)
          (parseEmail (cs txt))

      astxt :: Either String SAML.NameID
      astxt = do
        nm <- mkName (cs txt)
        SAML.mkNameID (SAML.mkUNameIDUnspecified (fromName nm)) Nothing Nothing Nothing

      err :: String
      err = "lenientlyParseSAMLNameID: " <> show (asxml, asemail, astxt, txt)

  either
    (const $ fail err)
    (pure . Just)
    (asxml <|> asemail <|> astxt)

emailFromSAML :: HasCallStack => SAMLEmail.Email -> Email
emailFromSAML = fromJust . parseEmail . SAMLEmail.render

emailToSAML :: HasCallStack => Email -> SAMLEmail.Email
emailToSAML = CI.original . fromRight (error "emailToSAML") . SAMLEmail.validate . toByteString

-- | FUTUREWORK(fisx): if saml2-web-sso exported the 'NameID' constructor, we could make this
-- function total without all that praying and hoping.
emailToSAMLNameID :: HasCallStack => Email -> SAML.NameID
emailToSAMLNameID = fromRight (error "impossible") . SAML.emailNameID . fromEmail

emailFromSAMLNameID :: HasCallStack => SAML.NameID -> Maybe Email
emailFromSAMLNameID nid = case nid ^. SAML.nameID of
  SAML.UNameIDEmail email -> Just . emailFromSAML . CI.original $ email
  _ -> Nothing

mkUserNameScim :: Maybe Text -> UAuthId Maybe Identity Maybe -> Either String Name
mkUserNameScim (Just n) = const $ mkName n
mkUserNameScim Nothing = mkName . runIdentity . uaScimExternalId

mkUserNameSaml :: Maybe Text -> UAuthId Identity Maybe Maybe -> Either String Name
mkUserNameSaml (Just n) = const $ mkName n
mkUserNameSaml Nothing = mkName . CI.original . SAML.unsafeShowNameID . view SAML.uidSubject . runIdentity . uaSamlId

-- | For testing.  Create a sample 'SAML.UserRef' value with random seeds to make 'Issuer' and
-- 'NameID' unique.
mkSampleUref :: Text -> Text -> SAML.UserRef
mkSampleUref iseed nseed = SAML.UserRef issuer nameid
  where
    issuer :: SAML.Issuer
    issuer = SAML.Issuer ([uri|http://example.com/|] & URI.pathL .~ cs ("/" </> cs iseed))

    nameid :: SAML.NameID
    nameid = fromRight (error ("impossible: " <> show (iseed, nseed))) $ do
      unqualified <- SAML.mkUNameIDEmail $ "me" <> nseed <> "@example.com"
      SAML.mkNameID unqualified Nothing Nothing Nothing

-- | @mkSampleUref "" ""@
mkSimpleSampleUref :: SAML.UserRef
mkSimpleSampleUref = mkSampleUref "" ""

-- | Another, more basic variant of `mkSampleUref`.
mkBasicSampleUref :: Text -> Text -> SAML.UserRef
mkBasicSampleUref issuer_ nameid_ = SAML.UserRef issuer nameid
  where
    issuer :: SAML.Issuer
    issuer = either (error . show) SAML.Issuer $ URI.parseURI URI.laxURIParserOptions (cs issuer_)

    nameid :: SAML.NameID
    nameid = either error id (SAML.mkNameID unqualified Nothing Nothing Nothing)
      where
        unqualified = fromRight (SAML.mkUNameIDUnspecified nameid_) (SAML.mkUNameIDEmail nameid_)
