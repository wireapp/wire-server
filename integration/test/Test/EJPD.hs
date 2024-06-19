{-# OPTIONS -Wno-ambiguous-fields #-}

module Test.EJPD
  ( testEJPDRequest,
    testEJPDRequestRemote,
  )
where

import API.Brig
import qualified API.BrigInternal as BI
import API.Galley
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
setupEJPD :: (HasCallStack) => App (A.Value, A.Value, A.Value, A.Value, A.Value)
setupEJPD =
  do
    (owner1, tid1, [usr1, usr2]) <- createTeam OwnDomain 3
    handle1 <- liftIO $ UUID.nextRandom <&> ("usr1-handle-" <>) . UUID.toString
    handle2 <- liftIO $ UUID.nextRandom <&> ("usr2-handle-" <>) . UUID.toString
    owner1Handle <- liftIO $ UUID.nextRandom <&> ("owner1-handle-" <>) . UUID.toString
    void $ putHandle usr1 handle1
    void $ putHandle usr2 handle2
    void $ putHandle owner1 owner1Handle
    email3 <- liftIO $ UUID.nextRandom <&> \uuid -> "usr3-" <> UUID.toString uuid <> "@example.com"
    email4 <- liftIO $ UUID.nextRandom <&> \uuid -> "usr4-" <> UUID.toString uuid <> "@example.com"
    email5 <- liftIO $ UUID.nextRandom <&> \uuid -> "usr5-" <> UUID.toString uuid <> "@example.com"
    usr3 <- randomUser OwnDomain def {BI.email = Just email3, BI.name = Just "usr3"}
    usr4 <- randomUser OwnDomain def {BI.email = Just email4, BI.name = Just "usr4"}
    usr5 <- randomUser OwnDomain def {BI.email = Just email5, BI.name = Just "usr5"}
    usrRemote <- randomUser OtherDomain def {BI.email = Nothing, BI.name = Just "usrRemote"}
    handle3 <- liftIO $ UUID.nextRandom <&> ("usr3-handle-" <>) . UUID.toString
    handle4 <- liftIO $ UUID.nextRandom <&> ("usr4-handle-" <>) . UUID.toString
    handle5 <- liftIO $ UUID.nextRandom <&> ("usr5-handle-" <>) . UUID.toString
    void $ putHandle usr3 handle3
    void $ putHandle usr4 handle4
    void $ putHandle usr5 handle5

    connectTwoUsers usr3 usr5
    connectUsers [usr2, usr4, usr5, usrRemote]

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
    assets2 <- (: []) . snd <$> uploadDownloadProfilePicture usr2
    assets3 <- (: []) . snd <$> uploadDownloadProfilePicture usr3
    assets4 <- (: []) . snd <$> uploadDownloadProfilePicture usr4

    (convs1, convs2, convs3, convs4, convs5) <- do
      let parse :: Response -> App Value
          parse resp =
            getJSON 201 resp <&> \val ->
              object
                [ "conv_name" .= do val ^?! key (fromString "name") . _String,
                  "conv_id" .= do val ^?! key (fromString "qualified_id") . _Object
                ]

      conv1 <-
        parse
          =<< postConversation usr1 do
            defMLS {name = Just "11", qualifiedUsers = [], team = Just tid1}
      conv12 <-
        parse
          =<< postConversation usr1 do
            defProteus {name = Just "12", qualifiedUsers = [usr2], team = Just tid1}
      conv35 <-
        parse
          =<< postConversation
            usr3
            do defProteus {name = Just "35", qualifiedUsers = [usr5]}
      conv524 <-
        parse
          =<< postConversation usr5 do
            defProteus {name = Just "524", qualifiedUsers = [usr2, usr4]}
      pure (Just ([conv1, conv12]), Just ([conv12, conv524]), Just [conv35], Just [conv524], Just [conv35, conv524])

    assertSuccess =<< postConversation usrRemote do
      defProteus {name = Just "remote245", qualifiedUsers = [usr2, usr4, usr5]}

    let usr2contacts = Just $ (,"accepted") <$> [ejpd4, ejpd5]
        usr3contacts = Just $ (,"accepted") <$> [ejpd5]
        usr4contacts = Just $ (,"accepted") <$> [ejpd2, ejpd5]
        usr5contacts = Just $ (,"accepted") <$> [ejpd2, ejpd3, ejpd4]

        ejpd0 = mkUsr owner1 (Just owner1Handle) [] Nothing (Just ([ejpd1, ejpd2], "list_complete")) Nothing Nothing
        ejpd1 = mkUsr usr1 (Just handle1) toks1 Nothing (Just ([ejpd0, ejpd2], "list_complete")) convs1 (Just assets1)
        ejpd2 = mkUsr usr2 (Just handle2) toks2 usr2contacts (Just ([ejpd0, ejpd1], "list_complete")) convs2 (Just assets2)
        ejpd3 = mkUsr usr3 (Just handle3) [] usr3contacts Nothing convs3 (Just assets3)
        ejpd4 = mkUsr usr4 (Just handle4) toks4 usr4contacts Nothing convs4 (Just assets4)
        ejpd5 = mkUsr usr5 (Just handle5) [] usr5contacts Nothing convs5 Nothing

    pure (ejpd1, ejpd2, ejpd3, ejpd4, ejpd5)
  where
    -- Return value is a 'EJPDResponseItem'.
    mkUsr ::
      (HasCallStack) =>
      A.Value {- user -} ->
      Maybe String {- handle (in case usr is not up to date, we pass this separately) -} ->
      [String {- push tokens -}] ->
      -- contacts
      Maybe [(A.Value {- ejpd response item of contact -}, String {- relation -})] ->
      -- team contacts
      Maybe ([A.Value {- ejpd response item -}], String {- pagination flag -}) ->
      -- conversations
      Maybe [A.Value] ->
      Maybe [String {- asset url -}] ->
      A.Value
    mkUsr usr hdl toks contacts teamContacts convs assets = result
      where
        result =
          object
            [ -- (We know we have "id", but using ^? instead of ^. avoids the need for a Monoid instance for Value.)
              "UserId" .= (usr ^? key (fromString "qualified_id")),
              "TeamId" .= (usr ^? key (fromString "team")),
              "Name" .= (usr ^? key (fromString "name")),
              "Handle" .= hdl,
              "Email" .= (usr ^? key (fromString "email")),
              "Phone" .= (usr ^? key (fromString "phone")),
              "PushTokens" .= toks,
              "Contacts"
                .= let f (item, relation) = object ["contact_item" .= item, "contact_relation" .= relation]
                    in (map (f . trimContact _1) <$> contacts),
              "TeamContacts"
                .= ( teamContacts
                       & maybe
                         Null
                         ( \(tcs, ltyp) ->
                             object
                               [ "TeamContacts" .= (trimContact id <$> tcs),
                                 "ListType" .= ltyp
                               ]
                         )
                   ),
              "Conversations" .= convs,
              "Assets" .= assets
            ]

    trimContact :: forall x. Lens' x A.Value -> x -> x
    trimContact lns =
      lns %~ \case
        trimmable@(A.Object _) -> trimItem trimmable
        other -> error $ show other

    trimItem :: A.Value -> A.Value
    trimItem =
      (key (fromString "Contacts") .~ A.Null)
        . (key (fromString "TeamContacts") .~ A.Null)
        . (key (fromString "Conversations") .~ A.Null)

testEJPDRequest :: (HasCallStack) => App ()
testEJPDRequest = do
  (usr1, usr2, usr3, usr4, usr5) <- setupEJPD

  let check :: (HasCallStack) => [A.Value] -> App ()
      check want = do
        let handle = cs . (^?! (key (fromString "Handle") . _String))
        have <- BI.getEJPDInfo OwnDomain (handle <$> want) "include_contacts"
        have.json `shouldMatchSpecial` object ["EJPDResponse" .= want]

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

testEJPDRequestRemote :: (HasCallStack) => App ()
testEJPDRequestRemote = do
  usrRemote <- randomUser OtherDomain def {BI.email = Nothing, BI.name = Just "usrRemote"}
  handleRemote <- liftIO $ UUID.nextRandom <&> UUID.toString
  assertSuccess =<< putHandle usrRemote handleRemote

  have <- BI.getEJPDInfo OwnDomain [handleRemote] "include_contacts"
  shouldBeEmpty $ have.json %. "EJPDResponse"
