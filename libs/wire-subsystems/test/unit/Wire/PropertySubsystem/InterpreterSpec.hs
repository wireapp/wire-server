{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.PropertySubsystem.InterpreterSpec where

import Data.Aeson (FromJSON, ToJSON, Value (..), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Text qualified as Aeson
import Data.Bifunctor (second)
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Range
import Data.Scientific (scientific)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Ascii (AsciiPrintable, AsciiText (..), validatePrintable)
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Encoding qualified as LText
import GHC.IsList (fromList)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.TinyLog
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Properties
import Wire.API.UserEvent
import Wire.Events
import Wire.MockInterpreters
import Wire.PropertyStore (PropertyStore)
import Wire.PropertySubsystem
import Wire.PropertySubsystem.Interpreter
import Wire.Sem.Logger.TinyLog (discardTinyLogs)

defaultConfig :: PropertySubsystemConfig
defaultConfig =
  PropertySubsystemConfig
    { maxKeyLength = 1024,
      maxValueLength = 1024,
      maxProperties = 16
    }

interpretDependencies :: Sem '[PropertyStore, Events, State [MiniEvent], TinyLog, Error e] a -> Either e a
interpretDependencies =
  run
    . runError
    . discardTinyLogs
    . evalState mempty
    . miniEventInterpreter
    . evalState mempty
    . inMemoryPropertyStoreInterpreter
    . raiseUnder

spec :: Spec
spec = do
  describe "Wire.PropertySubsystem.Interpreter" $ do
    prop "set/lookup property" $
      \uid connId key (SmallJSON val) ->
        let valBS = Aeson.encode val
            rawVal = RawPropertyValue valBS
            retrievedVal =
              interpretDependencies
                . interpretPropertySubsystem defaultConfig
                $ do
                  setProperty uid connId key rawVal
                  lookupProperty uid key
         in retrievedVal === Right (Just rawVal)

    prop "events" $ do
      \uid connId key (SmallJSON val) ->
        let valBS = Aeson.encode val
            rawVal = RawPropertyValue valBS
            assertion =
              interpretDependencies
                . interpretPropertySubsystem defaultConfig
                $ do
                  setProperty uid connId key rawVal
                  eventsAfterSet <- get

                  -- clear events
                  put []
                  deleteProperty uid connId key
                  eventsAfterDelete <- get

                  put []
                  clearProperties uid connId
                  eventsAfterClear <- get

                  -- assertions
                  pure $
                    eventsAfterSet === [MkMiniEvent uid (Just connId) $ PropertyEvent $ PropertySet key val]
                      .&&. eventsAfterDelete === [MkMiniEvent uid (Just connId) $ PropertyEvent $ PropertyDeleted key]
                      .&&. eventsAfterClear === [MkMiniEvent uid (Just connId) $ PropertyEvent PropertiesCleared]
         in either
              (\e -> counterexample ("UnexpectedError: " <> show e) False)
              id
              assertion

    prop "set/delete/lookup property" $
      \uid connId key (SmallJSON val) ->
        let valBS = Aeson.encode val
            rawVal = RawPropertyValue valBS
            retrievedVal = interpretDependencies . interpretPropertySubsystem defaultConfig $ do
              setProperty uid connId key rawVal
              deleteProperty uid connId key
              lookupProperty uid key
         in retrievedVal === Right Nothing

    prop "getAllProperties" $
      -- 16 is the default maxProperties
      \uid connId (fromRange @0 @16 @[(PropertyKey, SmallJSON)] -> keySmallVal) ->
        let keyVal = unwrapSmallJSON <$> Map.fromList keySmallVal
            keyValRaw = RawPropertyValue . Aeson.encode <$> keyVal
            retrievedVal =
              interpretDependencies
                . interpretPropertySubsystem defaultConfig
                $ do
                  forM_ (Map.toAscList keyValRaw) (uncurry (setProperty uid connId))
                  getAllProperties uid
         in retrievedVal === Right (PropertyKeysAndValues keyVal)

    prop "getPropertyKeys" $
      -- 16 is the default maxProperties
      \uid connId (fromRange @0 @16 @[(PropertyKey, SmallJSON)] -> keyVals) ->
        let keyValRaw = Map.fromList $ map (second (RawPropertyValue . Aeson.encode)) keyVals
            retrievedVal =
              interpretDependencies
                . interpretPropertySubsystem defaultConfig
                $ do
                  forM_ (Map.toAscList keyValRaw) (uncurry (setProperty uid connId))
                  getPropertyKeys uid
         in second Set.fromList retrievedVal === Right (Map.keysSet keyValRaw)

    prop "clearProperties" $
      -- 16 is the default maxProperties
      \uid connId (fromRange @0 @16 @[(PropertyKey, SmallJSON)] -> keyVals) ->
        let keyValRaw = Map.fromList $ map (second (RawPropertyValue . Aeson.encode)) keyVals
            retrievedVal =
              interpretDependencies
                . interpretPropertySubsystem defaultConfig
                $ do
                  forM_ (Map.toAscList keyValRaw) (uncurry (setProperty uid connId))
                  clearProperties uid connId
                  getAllProperties uid
         in retrievedVal === Right (PropertyKeysAndValues mempty)

    prop "setting non JSON values should result in an error" $
      -- 1024 is the default max value length
      \uid connId key (fromRange @0 @1024 @[Word8] -> nonJSONBytes) ->
        let nonJSONBS = LBS.pack nonJSONBytes
            setPropertyResult = interpretDependencies . interpretPropertySubsystem defaultConfig $ do
              setProperty uid connId key (RawPropertyValue nonJSONBS)
         in isNothing (Aeson.decode @Value nonJSONBS) ==>
              case setPropertyResult of
                Left (PropertyValueInvalid _) -> property True
                Left x -> counterexample ("Expected PropertyValueInvalid, got: " <> show x) False
                Right () -> counterexample ("Expected PropertyValueInvalid, but there was no error") False

    prop "setting very big JSON values should result in an error" $
      -- Capping default max value length to 1024 to make tests faster, bigger
      -- number => slower tests.
      \uid connId key (val :: Value) (fromIntegral . fromRange @0 @1024 @Int32 -> maxValueLength) ->
        let cfg = defaultConfig {maxValueLength = maxValueLength}
            -- Adding spaces to the end shouldn't change the meaning of a JSON,
            -- maybe there are better ways of generating a big JSON
            valBS =
              LText.encodeUtf8
                . LText.justifyLeft (fromIntegral $ maxValueLength + 1) ' '
                $ Aeson.encodeToLazyText val
            rawVal = RawPropertyValue valBS
            setPropertyResult = interpretDependencies . interpretPropertySubsystem cfg $ do
              setProperty uid connId key rawVal
         in setPropertyResult === Left PropertyValueTooLarge

    prop "setting very big key names should result in an error" $
      \uid connId (fromRange @1 @1024 @AsciiPrintable -> unwrappedKey) (val :: SmallJSON) ->
        let cfg = defaultConfig {maxKeyLength = (fromIntegral . Text.length $ toText unwrappedKey) - 1}
            valBS = Aeson.encode val
            rawVal = RawPropertyValue valBS
            setPropertyResult = interpretDependencies . interpretPropertySubsystem cfg $ do
              setProperty uid connId (PropertyKey unwrappedKey) rawVal
         in setPropertyResult === Left PropertyKeyTooLarge

    prop "setProperty should respect maxProperties config" $
      \uid connId keyPrefix (SmallJSON val) (fromIntegral . fromRange @1 @20 @Int32 -> maxProperties) ->
        let cfg = defaultConfig {maxProperties = maxProperties}
            mkKey n =
              let Right suffix = validatePrintable $ Text.pack $ show n
               in PropertyKey $ keyPrefix <> suffix
            keys = map mkKey [1 .. maxProperties]
            extraKey = mkKey (maxProperties + 1)
            valBS = Aeson.encode val
            rawVal = RawPropertyValue valBS
            assertion =
              interpretDependencies
                . interpretPropertySubsystem cfg
                $ do
                  forM_ keys $ \key -> setProperty uid connId key rawVal
                  setPropErr <- catchExpectedError $ setProperty uid connId extraKey rawVal
                  allProps <- getAllProperties uid
                  pure $
                    LBS.length valBS <= defaultConfig.maxValueLength ==>
                      setPropErr === Just TooManyProperties
                        .&&. allProps === PropertyKeysAndValues (Map.fromList (map (,val) keys))
         in either
              (\e -> counterexample ("UnexpectedError: " <> show e) False)
              id
              assertion

    prop "setProperty should work for pre-existing properties even when maxProperties is reached" $
      \uid connId keyPrefix (SmallJSON val) (SmallJSON newVal) (fromIntegral . fromRange @1 @20 @Int32 -> maxProperties) ->
        let cfg = defaultConfig {maxProperties = maxProperties}
            mkKey n =
              let Right suffix = validatePrintable $ Text.pack $ show n
               in PropertyKey $ keyPrefix <> suffix
            keys = map mkKey [1 .. maxProperties]
            rawVal = RawPropertyValue (Aeson.encode val)
            newRawVal = RawPropertyValue (Aeson.encode newVal)
            retrievedVal =
              interpretDependencies
                . interpretPropertySubsystem cfg
                $ do
                  forM_ keys $ \key -> setProperty uid connId key rawVal
                  setProperty uid connId (head keys) newRawVal
                  lookupProperty uid (head keys)
         in retrievedVal === Right (Just newRawVal)

  describe "arbitrary @SmallJSON" $
    -- Please run this at least a million times when something about it changes
    prop "Always produces JSON <= 1024 bytes" $
      \(smallJSON :: SmallJSON) ->
        let jsonStr = LText.unpack $ Aeson.encodeToLazyText smallJSON
            jsonBS = Aeson.encode smallJSON
         in counterexample ("length = " <> show (LBS.length jsonBS) <> "\n" <> jsonStr) $ LBS.length jsonBS <= 1024

newtype SmallJSON = SmallJSON {unwrapSmallJSON :: Value}
  deriving stock (Show, Eq)
  deriving newtype (FromJSON, ToJSON)

-- | generates small-ish JSON values
instance Arbitrary SmallJSON where
  arbitrary = SmallJSON <$> go 0
    where
      maxThings = 5
      -- ASCII chars take less space in the JSON
      genText = toText . fromRange <$> arbitrary @(Range 0 5 AsciiPrintable)
      go depth
        | depth >= maxThings = pure Null
        | otherwise = do
            chooseInt (0, 5) >>= \case
              0 -> String <$> genText
              1 -> Number <$> (scientific <$> chooseInteger (0, 1000) <*> chooseInt (-1, 2))
              2 -> Bool <$> arbitrary
              3 -> pure $ Null
              4 -> do
                n <- chooseInt (0, maxThings)
                Array . fromList <$> replicateM n (go (depth + 1))
              _ -> do
                n <- chooseInt (0, maxThings)
                keys <- Key.fromText <$$> replicateM n genText
                vals <- replicateM n $ go (depth + 1)
                pure . Aeson.object $ zipWith (.=) keys vals
