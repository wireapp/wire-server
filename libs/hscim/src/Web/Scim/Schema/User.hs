{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- | SCIM user representation.
--
-- = Our interpretation of the spec
--
-- The spec can be read at <https://tools.ietf.org/html/rfc7643#section-4.1>.
-- While implementing the spec we had to resolve some ambiguities and place some
-- additional constraints on the possible SCIM server behavior we can support.
--
-- == Resource ID / user ID
--
-- The 'User' object doesn't contain a user ID (as in "opaque server-assigned
-- immutable ID") by design. IDs and metadata are added to types in a uniform
-- fashion by using @WithId@ and @WithMeta@.
--
-- == Optional fields
--
-- The spec only mandates the @userName@ and @id@ attribute. All other
-- attributes seem optional.
--
-- == Multi-valued fields
--
-- When a multi-valued field (e.g. @emails@) doesn't contain any values, it's
-- unclear whether we should serialize it as @[]@ or omit it entirely. We have
-- opted for the latter to conform to an example in the spec:
-- <https://tools.ietf.org/html/rfc7644#section-3.5.1>.
--
-- TODO(arianvp):
--  Multi-valued attributes actually have some more quirky semantics that we
--  currently don't support yet. E.g. if the multi-values have a
--  'primary' field then only one of the entires must have 'primary: true'
--  and all the others are either implied 'primary: false' or must be checked
--  that they're false
--
--
-- == Attribute names
--
-- When parsing JSON objects, we ignore capitalization differences in field
-- names -- e.g. both @USERNAME@ and @userName@ are accepted.
--  This is described by the spec  https://tools.ietf.org/html/rfc7643#section-2.1
module Web.Scim.Schema.User
  ( User (..),
    empty,
    NoUserExtra (..),
    applyPatch,
    resultToScimError,
    isUserSchema,
    module Web.Scim.Schema.UserTypes,
  )
where

import Control.Monad
import Control.Monad.Except
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Lens.Micro
import qualified Text.Email.Validate as EmailValidate
import Web.Scim.AttrName
import Web.Scim.Filter
  ( AttrPath (..),
    CompValue (..),
    CompareOp (..),
    Filter (..),
    SubAttr (..),
    ValuePath (..),
    compareStr,
  )
import Web.Scim.Schema.Common
import Web.Scim.Schema.Error
import Web.Scim.Schema.PatchOp
import Web.Scim.Schema.Schema (Schema (..), getSchemaUri)
import Web.Scim.Schema.User.Address (Address)
import Web.Scim.Schema.User.Certificate (Certificate)
import Web.Scim.Schema.User.Email (Email (Email, primary, typ), EmailAddress (..))
import Web.Scim.Schema.User.Entitlement (Entitlement)
import Web.Scim.Schema.User.IM (IM)
import Web.Scim.Schema.User.Name (Name)
import Web.Scim.Schema.User.Phone (Phone)
import Web.Scim.Schema.User.Photo (Photo)
import Web.Scim.Schema.User.Role (Role)
import Web.Scim.Schema.UserTypes

-- | SCIM user record, parametrized with type-level @tag@ (see 'UserTypes').
data User tag = User
  { schemas :: [Schema],
    -- Mandatory fields
    userName :: Text,
    -- Optional fields
    externalId :: Maybe Text,
    name :: Maybe Name,
    displayName :: Maybe Text,
    nickName :: Maybe Text,
    profileUrl :: Maybe URI,
    title :: Maybe Text,
    userType :: Maybe Text,
    preferredLanguage :: Maybe Text,
    locale :: Maybe Text,
    active :: Maybe ScimBool,
    password :: Maybe Text,
    -- Multi-valued fields
    emails :: [Email],
    phoneNumbers :: [Phone],
    ims :: [IM],
    photos :: [Photo],
    addresses :: [Address],
    entitlements :: [Entitlement],
    roles :: [Role],
    x509Certificates :: [Certificate],
    -- Extra data.
    --
    -- During rendering, we'll convert it to JSON; if it's an object we'll merge it with the
    -- main user object, if it's @null@ we'll do nothing, otherwise we'll add it under the
    -- @"extra"@ field (though you should definitely not rely on this).
    --
    -- During parsing, we'll attempt to parse the /whole/ user object as @extra@, so your
    -- 'FromJSON' instance should be prepared to ignore unrelated fields. Also keep in mind that
    -- the SCIM spec requires field names to be case-insensitive, i.e. if you're looking for a
    -- field "foo" you should also handle a field called "FOO". Look at the @FromJSON User@
    -- instance to see how it can be done.
    --
    -- FUTUREWORK: make it easy for hscim users to implement a proper parser (with correct
    -- rendering of optional and multivalued fields, lowercase objects, etc).
    extra :: UserExtra tag
  }
  deriving (Generic)

deriving instance (Show (UserExtra tag)) => Show (User tag)

deriving instance (Eq (UserExtra tag)) => Eq (User tag)

empty ::
  -- | Schemas
  [Schema] ->
  -- | userName
  Text ->
  -- | Extra data
  UserExtra tag ->
  User tag
empty schemas userName extra =
  User
    { schemas = schemas,
      userName = userName,
      externalId = Nothing,
      name = Nothing,
      displayName = Nothing,
      nickName = Nothing,
      profileUrl = Nothing,
      title = Nothing,
      userType = Nothing,
      preferredLanguage = Nothing,
      locale = Nothing,
      active = Nothing,
      password = Nothing,
      emails = [],
      phoneNumbers = [],
      ims = [],
      photos = [],
      addresses = [],
      entitlements = [],
      roles = [],
      x509Certificates = [],
      extra = extra
    }

instance (FromJSON (UserExtra tag)) => FromJSON (User tag) where
  parseJSON = withObject "User" $ \obj -> do
    -- Lowercase all fields
    let o = KeyMap.fromList . map (over _1 lowerKey) . KeyMap.toList $ obj
    schemas <-
      o .:? "schemas" <&> \case
        Nothing -> [User20]
        Just xs -> if User20 `elem` xs then xs else User20 : xs
    userName <- o .: "username"
    externalId <- o .:? "externalid"
    name <- o .:? "name"
    displayName <- o .:? "displayname"
    nickName <- o .:? "nickname"
    profileUrl <- o .:? "profileurl"
    title <- o .:? "title"
    userType <- o .:? "usertype"
    preferredLanguage <- o .:? "preferredlanguage"
    locale <- o .:? "locale"
    active <- o .:? "active"
    password <- o .:? "password"
    emails <- o .:? "emails" .!= []
    phoneNumbers <- o .:? "phonenumbers" .!= []
    ims <- o .:? "ims" .!= []
    photos <- o .:? "photos" .!= []
    addresses <- o .:? "addresses" .!= []
    entitlements <- o .:? "entitlements" .!= []
    roles <- o .:? "roles" .!= []
    x509Certificates <- o .:? "x509certificates" .!= []
    extra <- parseJSON (Object obj)
    pure User {..}

instance (ToJSON (UserExtra tag)) => ToJSON (User tag) where
  toJSON User {..} =
    let mainObject =
          KeyMap.fromList $
            concat
              [ ["schemas" .= schemas],
                ["userName" .= userName],
                optionalField "externalId" externalId,
                optionalField "name" name,
                optionalField "displayName" displayName,
                optionalField "nickName" nickName,
                optionalField "profileUrl" profileUrl,
                optionalField "title" title,
                optionalField "userType" userType,
                optionalField "preferredLanguage" preferredLanguage,
                optionalField "locale" locale,
                optionalField "active" active,
                optionalField "password" password,
                multiValuedField "emails" emails,
                multiValuedField "phoneNumbers" phoneNumbers,
                multiValuedField "ims" ims,
                multiValuedField "photos" photos,
                multiValuedField "addresses" addresses,
                multiValuedField "entitlements" entitlements,
                multiValuedField "roles" roles,
                multiValuedField "x509Certificates" x509Certificates
              ]
        extraObject = case toJSON extra of
          Null -> mempty
          Object x -> x
          other -> KeyMap.fromList ["extra" .= other]
     in Object (KeyMap.union mainObject extraObject)
    where
      -- Omit a field if it's Nothing
      optionalField fname = \case
        Nothing -> []
        Just x -> [fname .= x]
      -- Omit a field if it's []
      multiValuedField fname = \case
        [] -> []
        xs -> [fname .= xs]

-- | A type used to indicate that the SCIM record doesn't have any extra data. Encoded as an
-- empty map.
data NoUserExtra = NoUserExtra
  deriving (Eq, Show)

instance FromJSON NoUserExtra where
  parseJSON = withObject "NoUserExtra" $ \_ -> pure NoUserExtra

instance ToJSON NoUserExtra where
  toJSON _ = object []

instance Patchable NoUserExtra where
  applyOperation _ _ = throwError $ badRequest InvalidValue (Just "there are no user extra attributes to patch")

----------------------------------------------------------------------------
-- Applying

-- | Applies a JSON Patch to a SCIM Core User
-- Only supports the core attributes.
-- Evenmore, only some hand-picked ones currently.
-- We'll have to think how patch is going to work in the presence of extensions.
-- Also, we can probably make  PatchOp type-safe to some extent (Read arianvp's thesis :))
applyPatch ::
  ( Patchable (UserExtra tag),
    FromJSON (UserExtra tag),
    MonadError ScimError m,
    UserTypes tag
  ) =>
  User tag ->
  PatchOp tag ->
  m (User tag)
applyPatch = (. getOperations) . foldM applyOperation

resultToScimError :: (MonadError ScimError m) => Result a -> m a
resultToScimError (Error reason) = throwError $ badRequest InvalidValue (Just (pack reason))
resultToScimError (Success a) = pure a

-- TODO(arianvp): support multi-valued and complex attributes.
-- TODO(arianvp): Actually do this in some kind of type-safe way. e.g.
-- have a UserPatch type.
--
-- What I understand from the spec:  The difference between add an replace is only
-- in the fact that replace will not concat multi-values, and behaves differently for complex values too.
-- For simple attributes, add and replace are identical.
applyUserOperation ::
  forall m tag.
  ( UserTypes tag,
    FromJSON (User tag),
    Patchable (UserExtra tag),
    MonadError ScimError m
  ) =>
  User tag ->
  Operation ->
  m (User tag)
applyUserOperation user (Operation Add path value) = applyUserOperation user (Operation Replace path value)
applyUserOperation user (Operation Replace (Just (NormalPath (AttrPath _schema attr _subAttr))) (Just value)) =
  case attr of
    "username" ->
      (\x -> user {userName = x}) <$> resultToScimError (fromJSON value)
    "displayname" ->
      (\x -> user {displayName = x}) <$> resultToScimError (fromJSON value)
    "externalid" ->
      (\x -> user {externalId = x}) <$> resultToScimError (fromJSON value)
    "active" ->
      (\x -> user {active = x}) <$> resultToScimError (fromJSON value)
    "roles" ->
      (\x -> user {roles = x}) <$> resultToScimError (fromJSON value)
    _ -> throwError (badRequest InvalidPath (Just "we only support attributes username, displayname, externalid, active, roles"))
applyUserOperation user (Operation Replace (Just (IntoValuePath vp mSub)) (Just val)) =
  case vp of
    ValuePath (AttrPath _ attr _) _
      | attr == "emails" -> replaceEmailsValuePath user vp mSub val
      | otherwise ->
          throwError
            ( badRequest
                InvalidPath
                (Just "multi-valued PATCH is only supported for 'emails'")
            )
applyUserOperation user (Operation Replace Nothing (Just value)) = do
  case value of
    Object hm | null ((AttrName . Key.toText <$> KeyMap.keys hm) \\ ["username", "displayname", "externalid", "active", "roles"]) -> do
      (u :: User tag) <- resultToScimError $ fromJSON value
      pure $
        user
          { userName = userName u,
            displayName = displayName u,
            externalId = externalId u,
            active = active u
          }
    _ -> throwError (badRequest InvalidPath (Just "we only support attributes username, displayname, externalid, active, roles"))
applyUserOperation _ (Operation Replace _ Nothing) =
  throwError (badRequest InvalidValue (Just "No value was provided"))
applyUserOperation _ (Operation Remove Nothing _) = throwError (badRequest NoTarget Nothing)
applyUserOperation user (Operation Remove (Just (NormalPath (AttrPath _schema attr _subAttr))) _value) =
  case attr of
    "username" -> throwError (badRequest Mutability Nothing)
    "displayname" -> pure $ user {displayName = Nothing}
    "externalid" -> pure $ user {externalId = Nothing}
    "active" -> pure $ user {active = Nothing}
    "roles" -> pure $ user {roles = []}
    _ -> pure user
applyUserOperation user (Operation Remove (Just (IntoValuePath vp _mSub)) _) =
  case vp of
    ValuePath (AttrPath _ attr _) _
      | attr == "emails" -> pure user {emails = removeMatchingEmails vp (emails user)}
      | otherwise ->
          throwError
            ( badRequest
                InvalidPath
                (Just "multi-valued PATCH is only supported for 'emails'")
            )

----------------------------------------------------------------------------
-- Multi-valued 'emails' value-path PATCH
--
-- Previously any value-path target (e.g. @emails[type eq "work"].value@) was
-- rejected with "can not lens into multi-valued attributes yet". We now support
-- value-path PATCH for the @emails@ attribute only -- the single multi-valued
-- attribute that Spar persists. Other multi-valued attributes
-- (@phoneNumbers@, @ims@, ...) remain unsupported and still fail as before.
--
-- NOTE on "create on absent": RFC 7644 §3.5.2.3 says a @Replace@ value-path
-- that matches nothing is a no-op. Entra, however, emits an @Add@ (rewritten to
-- @Replace@ in 'applyUserOperation') against @emails[type eq "work"].value@ to
-- provision the address, expecting the entry to be created if absent. Every
-- mainstream SCIM client/validator expects this create-on-absent behaviour for
-- the email value-path, so we deviate from the RFC here: when the filter is
-- @type eq <s>@ and no entry matches, we append
-- @Email { typ = Just s, value = newVal, primary = Nothing }@.

-- | The 'Filter' embedded in a 'ValuePath'.
valuePathFilter :: ValuePath -> Filter
valuePathFilter (ValuePath _ flt) = flt

-- | Textual form of an 'Email' address, for string comparison.
emailValueText :: Email -> Text
emailValueText (Email _ addr _) =
  decodeUtf8 (EmailValidate.toByteString (unEmailAddress addr))

-- | Does this 'Email' satisfy the given single-attribute 'Filter'? Supports the
-- sub-attributes Entra and the spec use: @type@, @value@, @primary@. Any
-- operator in 'compareStr's domain works for @type@\/@value@; @primary@ only
-- supports @eq@\/@ne@. Unknown sub-attributes or a mismatched 'CompValue' type
-- mean "no match".
emailMatches :: Filter -> Email -> Bool
emailMatches (FilterAttrCompare (AttrPath _ attr _) op cval) email
  | attr == "type" = case cval of
      ValString s -> compareStr op (fromMaybe "" (typ email)) s
      _ -> False
  | attr == "value" = case cval of
      ValString s -> compareStr op (emailValueText email) s
      _ -> False
  | attr == "primary" = case cval of
      ValBool b -> primaryMatches op b (primary email)
      _ -> False
  | otherwise = False

-- | Compare a @primary@ filter value. Only @eq@\/@ne@ are meaningful.
primaryMatches :: CompareOp -> Bool -> Maybe ScimBool -> Bool
primaryMatches op b mp = case op of
  OpEq -> mp == Just (ScimBool b)
  OpNe -> mp /= Just (ScimBool b)
  _ -> False

-- | If the filter is @type eq <s>@, return @Just s@; otherwise 'Nothing'.
-- Drives create-on-absent for the @.value@ sub-attribute (see note above).
filterTypeEq :: Filter -> Maybe Text
filterTypeEq (FilterAttrCompare (AttrPath _ attr _) OpEq (ValString s))
  | attr == "type" = Just s
filterTypeEq _ = Nothing

-- | Apply an update to each matching email. Never creates new entries.
setEmailField :: Filter -> (Email -> Email) -> [Email] -> [Email]
setEmailField flt update = map (\e -> if emailMatches flt e then update e else e)

-- | Set the address of an 'Email'. Uses positional construction to avoid the
-- bare 'value' selector, which is ambiguous (shared by 'Email', 'WithId' and
-- 'Operation').
setEmailAddress :: EmailAddress -> Email -> Email
setEmailAddress newAddr (Email t _ p) = Email t newAddr p

-- | Replace the @.value@ of every matching email. When nothing matches and the
-- filter is @type eq <s>@, append a new entry (create-on-absent; see note).
replaceEmailValue :: Filter -> EmailAddress -> [Email] -> [Email]
replaceEmailValue flt newAddr es
  | any (emailMatches flt) es = setEmailField flt (setEmailAddress newAddr) es
  | otherwise =
      case filterTypeEq flt of
        Just t -> es <> [Email (Just t) newAddr Nothing]
        Nothing -> es

-- | Replace each whole matching email with a new one; append if none match.
--
-- NOTE: every entry that matches the filter is overwritten with the same
-- @newEmail@, so a filter matching several entries (e.g. two with
-- @type eq "work"@, which Spar does not prevent) collapses them into
-- duplicates. In practice each @type@ has at most one entry (the only mapping
-- Entra uses), so this does not arise.
replaceEmailEntry :: Filter -> Email -> [Email] -> [Email]
replaceEmailEntry flt newEmail es
  | any (emailMatches flt) es = setEmailField flt (const newEmail) es
  | otherwise = es <> [newEmail]

-- | Decode the operation value as one or more emails. A bare object is treated
-- as a single-element list; an array is decoded as-is.
decodeEmails :: (MonadError ScimError m) => Value -> m [Email]
decodeEmails val = case fromJSON val of
  Success (es' :: [Email]) -> pure es'
  _ -> (: []) <$> resultToScimError (fromJSON val)

-- | Handle a @Replace@ on an @emails[...]@ value-path.
replaceEmailsValuePath ::
  (MonadError ScimError m) =>
  User tag ->
  ValuePath ->
  Maybe SubAttr ->
  Value ->
  m (User tag)
replaceEmailsValuePath user vp mSub val =
  let flt = valuePathFilter vp
      es = emails user
   in case mSub of
        Just (SubAttr sub)
          | sub == "value" -> do
              newAddr <- resultToScimError (fromJSON val)
              pure user {emails = replaceEmailValue flt newAddr es}
          | sub == "type" -> do
              t <- resultToScimError (fromJSON val)
              pure user {emails = setEmailField flt (\e -> e {typ = Just t}) es}
          | sub == "primary" -> do
              b <- resultToScimError (fromJSON val)
              pure user {emails = setEmailField flt (\e -> e {primary = Just b}) es}
          | otherwise ->
              throwError
                ( badRequest
                    InvalidPath
                    (Just "only the 'value', 'type' and 'primary' sub-attributes of 'emails' can be patched")
                )
        Nothing -> do
          newEmails <- decodeEmails val
          pure user {emails = foldr (replaceEmailEntry flt) es newEmails}

-- | Drop every email matching the value-path filter (used by @Remove@).
--
-- NOTE: a sub-attribute on the path (e.g. @emails[type eq "work"].value@) is
-- ignored -- @Remove@ always drops the whole matching entry. (Clearing just the
-- @.value@ is infeasible anyway, since 'Email.value' is non-nullable.)
removeMatchingEmails :: ValuePath -> [Email] -> [Email]
removeMatchingEmails vp = filter (not . emailMatches (valuePathFilter vp))

instance (UserTypes tag, FromJSON (User tag), Patchable (UserExtra tag)) => Patchable (User tag) where
  applyOperation user op@(Operation _ (Just (NormalPath (AttrPath schema _ _))) _)
    | isUserSchema schema = applyUserOperation user op
    | isSupportedCustomSchema schema = (\x -> user {extra = x}) <$> applyOperation (extra user) op
    | otherwise =
        throwError $ badRequest InvalidPath $ Just $ "we only support these schemas: " <> Text.intercalate ", " (map getSchemaUri (supportedSchemas @tag))
    where
      isSupportedCustomSchema = maybe False (`elem` supportedSchemas @tag)
  applyOperation user op = applyUserOperation user op

-- Omission of a schema for users is implicitly the core schema
-- TODO(arianvp): Link to part of the spec that claims this.
isUserSchema :: Maybe Schema -> Bool
isUserSchema = maybe True (== User20)
