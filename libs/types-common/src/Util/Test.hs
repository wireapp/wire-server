{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Util.Test where

import Data.Monoid
import Data.Tagged
import Data.Typeable
import Options.Applicative
import System.Environment
import Test.Tasty.Options

newtype IntegrationConfigFile = IntegrationConfigFile String
    deriving (Eq, Ord, Typeable)

instance IsOption IntegrationConfigFile where
    defaultValue = IntegrationConfigFile "/etc/wire/integration/integration.yaml"
    parseValue = fmap IntegrationConfigFile . safeRead
    optionName = return "integration-config"
    optionHelp = return "Integration config file to read from"
    optionCLParser =
      fmap IntegrationConfigFile $ strOption $
        (  short (untag (return 'i' :: Tagged IntegrationConfigFile Char))
        <> long  (untag (optionName :: Tagged IntegrationConfigFile String))
        <> help  (untag (optionHelp :: Tagged IntegrationConfigFile String))
        )

handleParseError :: (Show a) => Either a b -> IO (Maybe b)
handleParseError (Left err) = do
  putStrLn $ "Parse failed: " ++ show err ++ "\nFalling back to environment variables"
  pure Nothing
handleParseError (Right val) = pure $ Just val


-- | Read shell variable and, if present, call tasty with an extra trailing @["-p", pattern]@ on the
-- command line.  This means you can now do this
--
-- @
-- WIRE_TASTY_PATTERN='$NF == "post /register - 201 + no email"' make integration
-- @
--
-- See <https://github.com/feuerbach/tasty#patterns> for all the details.
withWireTastyPatternEnv :: IO () -> IO ()
withWireTastyPatternEnv m = do
    args <- getArgs
    addargs <- maybe id (\pat -> (<> ["-p", pat])) . lookup "WIRE_TASTY_PATTERN" <$> getEnvironment
    withArgs (addargs args) m
