{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Util.Test where

import Data.Tagged
import Imports
import Options.Applicative
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
      ( short (untag (return 'i' :: Tagged IntegrationConfigFile Char))
          <> long (untag (optionName :: Tagged IntegrationConfigFile String))
          <> help (untag (optionHelp :: Tagged IntegrationConfigFile String))
      )

handleParseError :: (Show a) => Either a b -> IO (Maybe b)
handleParseError (Left err) = do
  putStrLn $ "Parse failed: " ++ show err ++ "\nFalling back to environment variables"
  pure Nothing
handleParseError (Right val) = pure $ Just val
