{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | State transition machine DSL for running various operations on spar and maintaining
-- internal invariants.  At the time of writing this, supported operations on wire teams are:
-- add scim peer, remove scim peer, add saml idp.
--
-- See the test cases why all the abstractions and boilerplates may be worth it already, but
-- since the DSL embedding is deep, it is also straight-forward to generate random "programs"
-- and thus get property-based integration tests!
module Test.Spar.STM (testCreateIdpsAndScimsV7) where

import API.Common (defPassword)
import API.GalleyInternal (setTeamFeatureStatus)
import API.Nginz (login)
import API.Spar
import qualified Data.Map as Map
import qualified SAML2.WebSSO as SAML
import SetupHelpers
import Test.Spar
import Testlib.JSON
import Testlib.Prelude
import qualified Text.XML.DSig as SAML

-- | This is a bit silly, but it allows us to write more straight-forward code and still get
-- better error messages than "something went wrong in your code, please try again".
(!) :: (HasCallStack, Ord k, Show k, Show a) => Map k a -> k -> a
m ! k = case m Map.!? k of
  Nothing -> error $ "(!) failed: " <> show (m, k)
  Just a -> a

infixl 9 !

-- | Create a few saml IdPs and a few scim peers.  Randomize the order in which they are
-- created, and which peers / IdPs they are associated with.
testCreateIdpsAndScimsV7 :: (HasCallStack) => App ()
testCreateIdpsAndScimsV7 = do
  runSteps
    [ MkSaml (SamlRef "saml1") ExpectSuccess
    ]

  runSteps
    [ MkScim (ScimRef "scim1") Nothing ExpectSuccess,
      MkSaml (SamlRef "saml1") ExpectSuccess,
      MkSaml (SamlRef "saml2") ExpectSuccess,
      MkScim (ScimRef "scim2") (Just (SamlRef "saml1")) ExpectSuccess,
      -- two scims can be associated with one idp
      MkScim (ScimRef "scim3") (Just (SamlRef "saml1")) ExpectSuccess,
      MkScim (ScimRef "scim4") (Just (SamlRef "saml2")) ExpectSuccess,
      MkScim (ScimRef "scim5") Nothing ExpectSuccess
    ]

  -- two saml idps cannot associate with the same scim peer: it would be unclear which idp the
  -- next user is supposed to be provisioned for.  (not need to test, because it cannot be
  -- expressed in the API.)  but two scim can connect to the same saml:
  runSteps
    [ MkSaml (SamlRef "saml1") ExpectSuccess,
      MkScim (ScimRef "scim1") (Just (SamlRef "saml1")) ExpectSuccess,
      RmScim (ScimRef "scim1"),
      MkScim (ScimRef "scim2") (Just (SamlRef "saml1")) ExpectSuccess
    ]

newtype SamlRef = SamlRef {_unSamlRef :: String}
  deriving newtype (Eq, Show, Ord, ToJSON)

newtype ScimRef = ScimRef {unScimRef :: String}
  deriving newtype (Eq, Show, Ord, ToJSON)

newtype SamlId = SamlId {unSamlId :: String}
  deriving newtype (Eq, Show, Ord, ToJSON)

newtype ScimId = ScimId {unScimId :: String}
  deriving newtype (Eq, Show, Ord, ToJSON, ToJSONKey)

newtype ScimToken = ScimToken {unScimToken :: String}
  deriving newtype (Eq, Show, Ord, ToJSON)

-- | DSL with relevant api calls (not test cases).  This should make writing down different
-- test cases very concise and not cost any generality.
data Step
  = MkScim ScimRef (Maybe SamlRef) ExpectedResult
  | -- | `RmScim` has expected result: delete is idempotent.
    RmScim ScimRef
  | -- | you can't associate a saml idp with a existing scim peer when creating the idp.
    -- do that by replacing the scim token and associating the new one during creation.
    MkSaml SamlRef ExpectedResult
  deriving (Show)

data ExpectedResult = ExpectSuccess | ExpectFailure Int String
  deriving (Eq, Show, Generic)

data State = State
  { allIdps :: Map SamlRef SamlId,
    allIdpCredsById :: Map SamlId (SAML.IdPMetadata, SAML.SignPrivCreds),
    allScims :: Map ScimRef (ScimId, ScimToken),
    allScimAssocs :: Map ScimId SamlId
  }
  deriving (Eq, Show)

emptyState :: State
emptyState = State mempty mempty mempty mempty

runSteps :: (HasCallStack) => [Step] -> App ()
runSteps steps = do
  (owner, tid, []) <- createTeam OwnDomain 1
  void $ setTeamFeatureStatus owner tid "sso" "enabled"
  go owner tid emptyState steps
  where
    go :: Value -> String -> State -> [Step] -> App ()
    go _ _ _ [] = pure ()
    -- add scim
    go owner tid state (next@(MkScim scimRef mbSamlRef expected) : steps') = addFailureContext (show next) do
      let mIdPId = (state.allIdps !) <$> mbSamlRef

      let p = def {name = Just (unScimRef scimRef), idp = unSamlId <$> mIdPId}
      state' <- bindResponse (createScimToken owner p) $ \resp -> do
        case expected of
          ExpectSuccess -> validateScimRegistration state scimRef mIdPId resp
          ExpectFailure errStatus errLabel -> validateError resp errStatus errLabel $> state
      validateState owner tid state'
      go owner tid state' steps'
    -- add saml
    go owner tid state (next@(MkSaml samlRef expected) : steps') = addFailureContext (show next) do
      state' <- do
        (resp, creds) <- registerTestIdPWithMetaWithPrivateCreds owner
        case expected of
          ExpectSuccess -> validateSamlRegistration state samlRef resp creds
          ExpectFailure errStatus errLabel -> validateError resp errStatus errLabel $> state
      validateState owner tid state'
      go owner tid state' steps'
    -- remove scim
    go owner tid state (next@(RmScim scimRef) : steps') = addFailureContext (show next) do
      let (scimId, _) = state.allScims ! scimRef
      state' <- bindResponse (deleteScimToken owner (unScimId scimId)) $ \resp -> do
        resp.status `shouldMatchInt` 204
        pure
          $ state
            { allScims = Map.delete scimRef (allScims state),
              allScimAssocs = Map.delete scimId (allScimAssocs state)
            }
      validateState owner tid state'
      go owner tid state' steps'

    validateScimRegistration :: State -> ScimRef -> Maybe SamlId -> Response -> App State
    validateScimRegistration state scimRef mIdPId resp = do
      resp.status `shouldMatchInt` 200
      scimId <- resp.json %. "info.id" >>= asString
      tok <- resp.json %. "token" >>= asString
      pure
        $ state
          { allScims = Map.insert scimRef (ScimId scimId, ScimToken tok) (allScims state),
            allScimAssocs = maybe id (Map.insert (ScimId scimId)) mIdPId $ allScimAssocs state
          }

    validateSamlRegistration :: State -> SamlRef -> Response -> (SAML.IdPMetadata, SAML.SignPrivCreds) -> App State
    validateSamlRegistration state samlRef resp creds = do
      resp.status `shouldMatchInt` 201
      samlId <- resp.json %. "id" >>= asString
      pure
        $ state
          { allIdps = Map.insert samlRef (SamlId samlId) state.allIdps,
            allIdpCredsById = Map.insert (SamlId samlId) creds state.allIdpCredsById
          }

    validateState :: Value -> String -> State -> App ()
    validateState owner tid state = do
      allIdps <- getIdps owner >>= getJSON 200 >>= (%. "providers") >>= asList
      allScims <- getScimTokens owner >>= getJSON 200 >>= (%. "tokens") >>= asList

      do
        -- are all idps from spar in the local test state and vice versa?
        let allLocal = Map.elems state.allIdps
        allSpar <- ((%. "id") >=> asString) `traverse` allIdps
        allLocal `shouldMatchSet` allSpar

      do
        -- are all scim peers from spar in the local test state and vice versa?
        let allLocal = fst <$> Map.elems state.allScims
        allSpar <- (%. "id") `traverse` allScims
        allLocal `shouldMatchSet` allSpar

      do
        -- are all local associations the same as on spar?
        let toScimIdpPair tokInfo = do
              mIdp <- lookupField tokInfo "idp"
              case mIdp of
                Just idp -> Just <$> ((,) <$> (tokInfo %. "id" >>= asString) <*> asString idp)
                Nothing -> pure Nothing

        sparState <- Map.fromList . catMaybes <$> (toScimIdpPair `mapM` allScims)
        sparState `shouldMatch` state.allScimAssocs

      do
        -- login.
        -- (auto-provisioning with saml without scim is intentionally not tested.)
        for_ (Map.elems state.allScims) $ \(scimId, tok) -> do
          let mIdp :: Maybe (String {- id -}, (SAML.IdPMetadata, SAML.SignPrivCreds))
              mIdp = do
                i <- Map.lookup scimId state.allScimAssocs
                c <- Map.lookup i state.allIdpCredsById
                pure (unSamlId i, c)

          scimUser <- randomScimUser
          email <- scimUser %. "externalId" >>= asString
          uid <- bindResponse (createScimUser owner (unScimToken tok) scimUser) $ \resp -> do
            resp.status `shouldMatchInt` 201
            resp.json %. "id" >>= asString
          when (isNothing mIdp) $ do
            registerUser OwnDomain tid email

          maybe (loginWithPassword 200 scimUser) (loginWithSaml True tid scimUser) mIdp

          bindResponse (deleteScimUser owner (unScimToken tok) uid) $ \resp -> do
            resp.status `shouldMatchInt` 204

          maybe (loginWithPassword 403 scimUser) (loginWithSaml False tid scimUser) mIdp

    validateError :: Response -> Int -> String -> App ()
    validateError resp errStatus errLabel = do
      do
        resp.status `shouldMatchInt` errStatus
        resp.json %. "code" `shouldMatchInt` errStatus
        resp.json %. "label" `shouldMatch` errLabel

loginWithPassword :: (HasCallStack) => Int -> Value -> App ()
loginWithPassword expectedStatus scimUser = do
  email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
  bindResponse (login OwnDomain email defPassword) $ \resp -> do
    resp.status `shouldMatchInt` expectedStatus
