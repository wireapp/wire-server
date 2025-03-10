{-# LANGUAGE OverloadedStrings #-}

module SAML2.Util
  ( module SAML2.Util,
    module Text.XML.Util,
  )
where

import Control.Lens
import Control.Monad.Except
import Data.String.Conversions
import qualified Data.Text as ST
import Data.Typeable
import GHC.Stack
import Text.XML.Util
import URI.ByteString

renderURI :: URI -> ST
renderURI = cs . serializeURIRef'

parseURI' :: (MonadError String m) => ST -> m URI
parseURI' uri = either (die' (Just $ show uri) (Proxy @URI)) pure . parseURI laxURIParserOptions . cs . ST.strip $ uri

-- | You probably should not use this.  If you have a string literal, consider "URI.ByteString.QQ".
unsafeParseURI :: ST -> URI
unsafeParseURI = either (error . ("could not parse config: " <>) . show) id . parseURI'

-- | @uriSegments "/one/two" == uriSegments "one/two/" == uriSegments "///one//two///" == ["one", "two"]@
uriSegments :: ST -> [ST]
uriSegments = filter (not . ST.null) . ST.splitOn "/"

uriUnSegments :: [ST] -> ST
uriUnSegments = ("/" <>) . ST.intercalate "/"

(-/) :: (HasCallStack) => ST -> ST -> ST
oldpath -/ pathext = uriUnSegments . uriSegments $ oldpath <> "/" <> pathext

(=/) :: (HasCallStack) => URI -> ST -> URI
uri =/ pathext = normURI $ uri & pathL %~ (<> "/" <> cs pathext)

normURI :: URI -> URI
normURI =
  unsafeParseURI
    . cs
    . normalizeURIRef'
      URINormalizationOptions
        { unoDowncaseScheme = True,
          unoDowncaseHost = True,
          unoDropDefPort = False,
          unoSlashEmptyPath = True,
          unoDropExtraSlashes = True,
          unoSortParameters = True,
          unoRemoveDotSegments = True,
          unoDefaultPorts = mempty
        }
