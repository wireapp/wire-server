{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--
module Web.Scim.Schema.User where

import Control.Monad (foldM)
import Data.Aeson
import Data.List ((\\))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack, toLower)
import Lens.Micro

import Web.Scim.AttrName
import Web.Scim.Filter (AttrPath(..))
import Web.Scim.Schema.Common
import Web.Scim.Schema.Schema (Schema(..))
import Web.Scim.Schema.User.Address (Address)
import Web.Scim.Schema.User.Certificate (Certificate)
import Web.Scim.Schema.User.Email (Email)
import Web.Scim.Schema.User.IM (IM)
import Web.Scim.Schema.User.Name (Name)
import Web.Scim.Schema.User.Phone (Phone)
import Web.Scim.Schema.User.Photo (Photo)
import Web.Scim.Schema.PatchOp
import Web.Scim.Schema.Error

import GHC.Generics (Generic)
import Control.Monad.Except

-- | Configurable parts of 'User'.
class UserTypes tag where
  -- | User ID type.
  type UserId tag
  -- | Extra data carried with each 'User'.
  type UserExtra tag

-- | SCIM user record, parametrized with type-level tag @t@ (see 'UserTypes').
data User tag = User
  {
    schemas :: [Schema]

  -- Mandatory fields
  , userName :: Text

  -- Optional fields
  , externalId :: Maybe Text
  , name :: Maybe Name
  , displayName :: Maybe Text
  , nickName :: Maybe Text
  , profileUrl :: Maybe URI
  , title :: Maybe Text
  , userType :: Maybe Text
  , preferredLanguage :: Maybe Text
  , locale :: Maybe Text
  , active :: Maybe Bool
  , password :: Maybe Text

  -- Multi-valued fields
  , emails :: [Email]
  , phoneNumbers :: [Phone]
  , ims :: [IM]
  , photos :: [Photo]
  , addresses :: [Address]
  , entitlements :: [Text]
  , roles :: [Text]
  , x509Certificates :: [Certificate]

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
  , extra :: UserExtra tag
  } deriving (Generic)

deriving instance Show (UserExtra tag) => Show (User tag)
deriving instance Eq (UserExtra tag) => Eq (User tag)


empty
  :: [Schema]               -- ^ Schemas
  -> Text                   -- ^ userName
  -> UserExtra tag          -- ^ Extra data
  -> User tag
empty schemas userName extra = User
  { schemas = schemas
  , userName = userName 
  , externalId = Nothing
  , name = Nothing
  , displayName = Nothing
  , nickName = Nothing
  , profileUrl = Nothing
  , title = Nothing
  , userType = Nothing
  , preferredLanguage = Nothing
  , locale = Nothing
  , active = Nothing
  , password = Nothing
  , emails = []
  , phoneNumbers = []
  , ims = []
  , photos = []
  , addresses = []
  , entitlements = []
  , roles = []
  , x509Certificates = []
  , extra = extra
  }

instance FromJSON (UserExtra tag) => FromJSON (User tag) where
  parseJSON = withObject "User" $ \obj -> do
    -- Lowercase all fields
    let o = HM.fromList . map (over _1 toLower) . HM.toList $ obj
    schemas <- o .:? "schemas" <&> \case
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
    pure User{..}

instance ToJSON (UserExtra tag) => ToJSON (User tag) where
  toJSON User{..} =
    let mainObject = HM.fromList $ concat
          [ [ "schemas" .= schemas ]
          , [ "userName" .= userName ]
          , optionalField "externalId" externalId
          , optionalField "name" name
          , optionalField "displayName" displayName
          , optionalField "nickName" nickName
          , optionalField "profileUrl" profileUrl
          , optionalField "title" title
          , optionalField "userType" userType
          , optionalField "preferredLanguage" preferredLanguage
          , optionalField "locale" locale
          , optionalField "active" active
          , optionalField "password" password
          , multiValuedField "emails" emails
          , multiValuedField "phoneNumbers" phoneNumbers
          , multiValuedField "ims" ims
          , multiValuedField "photos" photos
          , multiValuedField "addresses" addresses
          , multiValuedField "entitlements" entitlements
          , multiValuedField "roles" roles
          , multiValuedField "x509Certificates" x509Certificates
          ]
        extraObject = case toJSON extra of
          Null -> mempty
          Object x -> x
          other -> HM.fromList ["extra" .= other]
    in Object (HM.union mainObject extraObject)

    where
      -- Omit a field if it's Nothing
      optionalField fname = \case
        Nothing -> []
        Just x  -> [fname .= x]
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

----------------------------------------------------------------------------
-- Applying

-- | Applies a JSON Patch to a SCIM Core User
-- Only supports the core attributes.
-- Evenmore, only some hand-picked ones currently.
-- We'll have to think how patch is going to work in the presence of extensions.
-- Also, we can probably make  PatchOp type-safe to some extent (Read arianvp's thesis :))
applyPatch :: (FromJSON (UserExtra tag), MonadError ScimError m) => User tag  -> PatchOp ->  m (User tag)
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
applyOperation :: forall m tag. (MonadError ScimError m, FromJSON (UserExtra tag)) =>  User tag  -> Operation -> m (User tag)
applyOperation user (Operation Add path value) = applyOperation user (Operation Replace path value)

applyOperation user (Operation Replace (Just (NormalPath (AttrPath _schema attr _subAttr))) (Just value)) = do
  case attr of
    "username" ->
      (\x -> user { userName = x }) <$> resultToScimError (fromJSON value)
    "displayname" ->
      (\x -> user { displayName = x }) <$> resultToScimError (fromJSON value)
    "externalid" ->
      (\x -> user { externalId = x }) <$> resultToScimError (fromJSON value)
    _ -> throwError (badRequest InvalidPath (Just "we only support attributes username, displayname, externalid"))
applyOperation _ (Operation Replace (Just (IntoValuePath _ _)) _) = do
  throwError (badRequest InvalidPath (Just "can not lens into multi-valued attributes yet"))
applyOperation user (Operation Replace Nothing (Just value)) = do
  case value of
    Object hm | null ((AttrName <$> HM.keys hm) \\ ["username", "displayname", "externalid"]) -> pure ()
    _ -> throwError (badRequest InvalidPath (Just "we only support attributes username, displayname, externalid"))
  (u :: User tag) <- resultToScimError $ fromJSON value
  pure $ user
    { userName = userName u
    , displayName = displayName u
    , externalId = externalId u
    }
applyOperation _ (Operation Replace _ Nothing) =
  throwError (badRequest InvalidValue (Just "No value was provided"))

applyOperation _ (Operation Remove Nothing _) = throwError (badRequest NoTarget Nothing)
applyOperation user (Operation Remove (Just (NormalPath (AttrPath _schema attr _subAttr))) _value) =
  case attr of
    "username" -> throwError (badRequest Mutability Nothing)
    "displayname" -> pure $ user { displayName = Nothing }
    "externalid" -> pure $ user { externalId = Nothing }
    _ -> pure user
applyOperation _ (Operation Remove (Just (IntoValuePath _ _)) _) = do
  throwError (badRequest InvalidPath (Just "can not lens into multi-valued attributes yet"))


-- Omission of a schema for users is implicitly the core schema
-- TODO(arianvp): Link to part of the spec that claims this.
isUserSchema :: Maybe Schema -> Bool
isUserSchema Nothing = True
isUserSchema (Just User20) = True
isUserSchema (Just _) = False
