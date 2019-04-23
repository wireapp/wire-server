module API.Settings  where

import           Imports
import           Bilge              hiding (accept, timeout)
import           Brig.Options (Opts)
import           Brig.Run (mkApp)
import           Data.Barbie        (buniq)
import           Test.Tasty         hiding (Timeout)
import           Util
import           API.Team.Util
import qualified Galley.Types.Teams as Team

import Data.Id
import qualified Brig.Options as Opt

import Bilge.Assert
import Brig.Types
import Control.Arrow ((&&&))
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.ByteString.Conversion
import Test.Tasty.HUnit

import qualified Network.Wai.Test as WaiTest

import qualified Data.ByteString.Char8       as C8
import qualified Data.Set                    as Set

withCustomOptions :: Opts -> WaiTest.Session a -> IO a
withCustomOptions opts sess = do
    (app, _) <- mkApp opts
    WaiTest.runSession sess app

tests :: Opts -> Manager -> Brig -> Galley -> IO TestTree
tests defOpts manager brig galley = return $ do
    testGroup "settings"
        [ testGroup "setEmailVisibility"
            [ testGroup "/users/"
                [
                testCase "EmailVisibleIfOnTeam"
                . runHttpT manager
                $ testUsersEmailVisibleIfExpected defOpts brig galley Opt.EmailVisibleIfOnTeam
                , testCase "EmailVisibleToSelf"
                . runHttpT manager
                $ testUsersEmailVisibleIfExpected defOpts brig galley Opt.EmailVisibleToSelf
                ]
            , testGroup "/users/:id"
                [ testCase "EmailVisibleIfOnTeam"
                . runHttpT manager
                $ testGetUserEmailShowsEmailsIfExpected defOpts brig galley Opt.EmailVisibleIfOnTeam

                , testCase "EmailVisibleToSelf"
                . runHttpT manager
                $ testGetUserEmailShowsEmailsIfExpected defOpts brig galley Opt.EmailVisibleToSelf
                ]
            ]
        ]

data UserRelationship = SameTeam | DifferentTeam | NoTeam

expectEmailVisible :: Opt.EmailVisibility -> UserRelationship -> Bool
expectEmailVisible Opt.EmailVisibleIfOnTeam SameTeam = True
expectEmailVisible Opt.EmailVisibleIfOnTeam DifferentTeam = True
expectEmailVisible Opt.EmailVisibleIfOnTeam NoTeam = False

expectEmailVisible Opt.EmailVisibleToSelf SameTeam = False
expectEmailVisible Opt.EmailVisibleToSelf DifferentTeam = False
expectEmailVisible Opt.EmailVisibleToSelf NoTeam = False

jsonField :: FromJSON a => Text -> Value -> Maybe a
jsonField f u = u ^? key f >>= maybeFromJSON

testUsersEmailVisibleIfExpected :: Opts -> Brig -> Galley -> Opt.EmailVisibility -> Http ()
testUsersEmailVisibleIfExpected opts brig galley visibilitySetting = do
    (creatorId, tid) <- createUserWithTeam brig galley
    (otherTeamCreatorId, otherTid) <- createUserWithTeam brig galley
    userA <- createTeamMember brig galley creatorId tid Team.fullPermissions
    userB <- createTeamMember brig galley otherTeamCreatorId otherTid Team.fullPermissions
    nonTeamUser <- createUser "joe" brig
    let uids =
            C8.intercalate ","
            $ toByteString' <$> [userId userA, userId userB, userId nonTeamUser]
        expected :: Set (Maybe UserId, Maybe Email)
        expected =
            Set.fromList [ ( Just $ userId userA
                           , if expectEmailVisible visibilitySetting SameTeam
                                 then userEmail userA
                                 else Nothing
                           )
                         , ( Just $ userId userB
                           , if expectEmailVisible visibilitySetting DifferentTeam
                                 then userEmail userB
                                 else Nothing
                           )
                         , ( Just $ userId nonTeamUser
                           , if expectEmailVisible visibilitySetting NoTeam
                                 then userEmail nonTeamUser
                                 else Nothing
                           )
                         ]
    (brigApp, _) <- liftIO
        $ mkApp (opts
                 { Opt.optMutableSettings = (Opt.MutableSettings (Identity visibilitySetting))
                 })
    liftIO $ flip WaiTest.runSession brigApp $ do
        get (brig . zUser (userId userB) . path "users" . queryItem "ids" uids) !!! do
            const 200 === statusCode
            const (Just expected) === result
  where
    result r = Set.fromList . map (jsonField "id" &&& jsonField "email") <$> decodeBody r

testGetUserEmailShowsEmailsIfExpected :: Opts -> Brig -> Galley -> Opt.EmailVisibility -> Http ()
testGetUserEmailShowsEmailsIfExpected opts brig galley visibilitySetting = do
    (creatorId, tid) <- createUserWithTeam brig galley
    (otherTeamCreatorId, otherTid) <- createUserWithTeam brig galley
    userA <- createTeamMember brig galley creatorId tid Team.fullPermissions
    userB <- createTeamMember brig galley otherTeamCreatorId otherTid Team.fullPermissions
    nonTeamUser <- createUser "joe" brig
    let expectations :: [(UserId, Maybe Email)]
        expectations =
            [ (userId userA, if expectEmailVisible visibilitySetting SameTeam then userEmail userA
                                                         else Nothing)
            , (userId userB, if expectEmailVisible visibilitySetting DifferentTeam then userEmail userB
                                                              else Nothing)
            , (userId nonTeamUser, if expectEmailVisible visibilitySetting NoTeam then userEmail nonTeamUser
                                                             else Nothing)
            ]

    (brigApp, _) <- liftIO
        $ mkApp (opts
                 { Opt.optMutableSettings = (Opt.MutableSettings (Identity visibilitySetting))
                 })
    liftIO $ flip WaiTest.runSession brigApp $ do
        forM_ expectations $ \(uid, expectedEmail) ->
            get (brig . zUser (userId userB) . paths ["users", toByteString' uid]) !!! do
                const 200 === statusCode
                const expectedEmail === emailResult
  where
    emailResult :: Response (Maybe LByteString) -> Maybe Email
    emailResult r = decodeBody r >>= jsonField "email"


withEmailVisibility :: Opt.EmailVisibility -> Brig -> Http () -> Http ()
withEmailVisibility emailVisibilityOverride brig t =
    withSettingsOverrides brig newSettings t
  where
    newSettings :: Opt.MutableSettings' Maybe
    newSettings =
        (buniq Nothing)
        { Opt.setEmailVisibility = Just emailVisibilityOverride
        }
