{-# OPTIONS -Wno-ambiguous-fields #-}
module Test.EJPD (testEJPDRequest) where

import API.Brig
import qualified API.BrigInternal as BI
import API.Gundeck
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Data.String.Conversions (cs)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.Wreq as Wreq
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude

-- | Create some teams & users, and return their expected ejpd response values.
setupEJPD :: HasCallStack => App (A.Value, A.Value, A.Value, A.Value, A.Value)
setupEJPD =
  do
    (owner1, _tid1, [usr1, usr2]) <- createTeam OwnDomain 3
    handle1 <- liftIO $ UUID.nextRandom <&> ("usr1-handle-" <>) . UUID.toString
    handle2 <- liftIO $ UUID.nextRandom <&> ("usr2-handle-" <>) . UUID.toString
    void $ putHandle usr1 handle1
    void $ putHandle usr2 handle2
    email3 <- liftIO $ UUID.nextRandom <&> \uuid -> "usr3-" <> UUID.toString uuid <> "@example.com"
    email4 <- liftIO $ UUID.nextRandom <&> \uuid -> "usr4-" <> UUID.toString uuid <> "@example.com"
    email5 <- liftIO $ UUID.nextRandom <&> \uuid -> "usr5-" <> UUID.toString uuid <> "@example.com"
    usr3 <- randomUser OwnDomain def {BI.email = Just email3, BI.name = Just "usr3"}
    usr4 <- randomUser OwnDomain def {BI.email = Just email4, BI.name = Just "usr4"}
    usr5 <- randomUser OwnDomain def {BI.email = Just email5, BI.name = Just "usr5"}
    handle3 <- liftIO $ UUID.nextRandom <&> ("usr3-handle-" <>) . UUID.toString
    handle4 <- liftIO $ UUID.nextRandom <&> ("usr4-handle-" <>) . UUID.toString
    handle5 <- liftIO $ UUID.nextRandom <&> ("usr5-handle-" <>) . UUID.toString
    void $ putHandle usr3 handle3
    void $ putHandle usr4 handle4
    void $ putHandle usr5 handle5

    connectTwoUsers usr3 usr5
    connectTwoUsers usr2 usr4
    connectTwoUsers usr4 usr5

    toks1 <- do
      cl11 <- objId $ addClient (usr1 %. "qualified_id") def >>= getJSON 201
      bindResponse (generateAndPostPushToken usr1 cl11 def) $ \resp -> do
        resp.status `shouldMatchInt` 201
        tok <- resp.json %. "token" & asString
        pure [tok]
    toks2 <- do
      cl21 <- objId $ addClient (usr2 %. "qualified_id") def >>= getJSON 201
      cl22 <- objId $ addClient (usr2 %. "qualified_id") def >>= getJSON 201
      t1 <- bindResponse (generateAndPostPushToken usr2 cl21 def) $ \resp -> do
        resp.status `shouldMatchInt` 201
        resp.json %. "token" & asString
      t2 <- bindResponse (generateAndPostPushToken usr2 cl22 def) $ \resp -> do
        resp.status `shouldMatchInt` 201
        resp.json %. "token" & asString
      pure [t1, t2]
    toks4 <- do
      cl41 <- objId $ addClient (usr4 %. "qualified_id") def >>= getJSON 201
      bindResponse (generateAndPostPushToken usr4 cl41 def) $ \resp -> do
        resp.status `shouldMatchInt` 201
        tok <- resp.json %. "token" & asString
        pure [tok]

    assets1 <- do
      a1 <- uploadDownloadProfilePicture usr1
      a2 <- uploadDownloadProfilePicture usr1
      pure $ snd <$> [a1, a2]
    assets2 <- do
      (: []) . snd <$> uploadDownloadProfilePicture usr2
    assets3 <- do
      (: []) . snd <$> uploadDownloadProfilePicture usr3
    assets4 <- do
      (: []) . snd <$> uploadDownloadProfilePicture usr4

    (convs1, convs2, convs4) <- do
      -- FUTUREWORKI(fisx): implement this (create both team convs and regular convs)
      pure (Nothing, Nothing, Nothing)

    let usr2contacts = Just $ (,"accepted") <$> [ejpd4]
        usr3contacts = Just $ (,"accepted") <$> [ejpd5]
        usr4contacts = Just $ (,"accepted") <$> [ejpd2, ejpd5]
        usr5contacts = Just $ (,"accepted") <$> [ejpd3, ejpd4]

        ejpd0 = mkUsr owner1 Nothing [] Nothing (Just ([ejpd1, ejpd2], "list_complete")) Nothing Nothing
        ejpd1 = mkUsr usr1 (Just handle1) toks1 Nothing (Just ([ejpd0, ejpd2], "list_complete")) convs1 (Just assets1)
        ejpd2 = mkUsr usr2 (Just handle2) toks2 usr2contacts (Just ([ejpd0, ejpd1], "list_complete")) convs2 (Just assets2)
        ejpd3 = mkUsr usr3 (Just handle3) [] usr3contacts Nothing Nothing (Just assets3)
        ejpd4 = mkUsr usr4 (Just handle4) toks4 usr4contacts Nothing convs4 (Just assets4)
        ejpd5 = mkUsr usr5 (Just handle5) [] usr5contacts Nothing Nothing Nothing

    pure (ejpd1, ejpd2, ejpd3, ejpd4, ejpd5)
  where
    -- Return value is a 'EJPDResponseItem'.
    mkUsr ::
      HasCallStack =>
      A.Value {- user -} ->
      Maybe String {- handle (in case usr is not up to date, we pass this separately) -} ->
      [String {- push tokens -}] ->
      Maybe [(A.Value {- ejpd response item of contact -}, String {- relation -})] ->
      Maybe ([A.Value {- ejpd response item -}], String {- pagination flag -}) ->
      Maybe [(String {- conv name -}, String {- conv id -})] ->
      Maybe [String {- asset url -}] ->
      A.Value
    mkUsr usr handle toks contacts teamContacts convs assets = result
      where
        result =
          object
            [ -- (We know we have "id", but using ^? instead of ^. avoids the need for a Monoid instance for Value.)
              "ejpd_response_user_id" .= (usr ^? key (fromString "id")),
              "ejpd_response_team_id" .= (usr ^? key (fromString "team")),
              "ejpd_response_name" .= (usr ^? key (fromString "name")),
              "ejpd_response_handle" .= handle,
              "ejpd_response_email" .= (usr ^? key (fromString "email")),
              "ejpd_response_phone" .= (usr ^? key (fromString "phone")),
              "ejpd_response_push_tokens" .= toks,
              "ejpd_response_contacts" .= (trimContacts _1 <$> contacts),
              "ejpd_response_team_contacts" .= (teamContacts & _Just . _1 %~ trimContacts id),
              "ejpd_response_conversations" .= convs,
              "ejpd_response_assets" .= assets
            ]

    trimContacts :: forall x. Lens' x A.Value -> [x] -> [x]
    trimContacts lns =
      fmap
        ( lns
            %~ ( \case
                   trimmable@(A.Object _) -> trimItem trimmable
                   other -> error $ show other
               )
        )

    trimItem :: A.Value -> A.Value
    trimItem =
      (key (fromString "ejpd_response_contacts") .~ A.Null)
        . (key (fromString "ejpd_response_team_contacts") .~ A.Null)
        . (key (fromString "ejpd_response_conversations") .~ A.Null)

testEJPDRequest :: HasCallStack => App ()
testEJPDRequest = do
  (usr1, usr2, usr3, usr4, usr5) <- setupEJPD

  let check :: HasCallStack => [A.Value] -> App ()
      check want = do
        let handle = cs . (^?! (key (fromString "ejpd_response_handle") . _String))
        have <- BI.getEJPDInfo OwnDomain (handle <$> want) "include_contacts"
        have.json `shouldMatchSpecial` object ["ejpd_response" .= want]

      shouldMatchSpecial :: (MakesValue a, MakesValue b, HasCallStack) => a -> b -> App ()
      shouldMatchSpecial = shouldMatchWithRules [minBound ..] resolveAssetLinks

      -- query params and even the uuid in the path of asset urls may differ between actual
      -- and expected value because they are re-generated non-deterministically.  so we fetch
      -- the actual content.
      resolveAssetLinks :: A.Value -> App (Maybe A.Value)
      resolveAssetLinks = \case
        (A.String (cs -> url)) | isProbablyAssetUrl url -> (Just . toJSON) <$> fetchIt url
        _ -> pure Nothing
        where
          isProbablyAssetUrl :: String -> Bool
          isProbablyAssetUrl url = all (`isInfixOf` url) ["http", "://", "/dummy-bucket/v3/persistent/"]

          fetchIt :: String -> App String
          fetchIt url = liftIO $ (cs . view Wreq.responseBody) <$> Wreq.get url

  check [usr1]
  check [usr2]
  check [usr3]
  check [usr4, usr5]
