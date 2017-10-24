{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Util.Test where

import Data.Monoid
import Data.Proxy
import Data.Tagged
import Data.Typeable
import Options.Applicative
import Test.Tasty
import Test.Tasty.Options

newtype ServiceConfigFile = ServiceConfigFile String
    deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
    defaultValue = ServiceConfigFile "/etc/wire/service/conf/service.yaml"
    parseValue = fmap ServiceConfigFile . safeRead
    optionName = return "service-config-file"
    optionHelp = return "Service config file to read from"
    optionCLParser =
      fmap ServiceConfigFile $ strOption $
        (  long (untag (optionName :: Tagged ServiceConfigFile String))
        <> help (untag (optionHelp :: Tagged ServiceConfigFile String))
        )

newtype IntegrationConfigFile = IntegrationConfigFile String
    deriving (Eq, Ord, Typeable)

instance IsOption IntegrationConfigFile where
    defaultValue = IntegrationConfigFile "/etc/wire/conf/integration.yaml"
    parseValue = fmap IntegrationConfigFile . safeRead
    optionName = return "integration-config-file"
    optionHelp = return "Integration config file to read from"
    optionCLParser =
      fmap IntegrationConfigFile $ strOption $
        (  long (untag (optionName :: Tagged IntegrationConfigFile String))
        <> help (untag (optionHelp :: Tagged IntegrationConfigFile String))
        )

runTests :: (String -> String -> TestTree) -> IO ()
runTests run = defaultMainWithIngredients ings $ 
    askOption $ \(ServiceConfigFile c) ->
    askOption $ \(IntegrationConfigFile i) -> run c i        
  where
    ings =
      includingOptions 
        [Option (Proxy :: Proxy ServiceConfigFile)
        ,Option (Proxy :: Proxy IntegrationConfigFile)
        ]
      : defaultIngredients

handleParseError :: (Show a) => Either a b -> IO (Maybe b)
handleParseError (Left err) = do
  putStrLn $ "Parse failed: " ++ show err ++ "\nFalling back to environment variables"
  pure Nothing
handleParseError (Right val) = pure $ Just val
