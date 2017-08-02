{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Common templating utilities.
module Brig.Template
    ( -- * Reading templates
      Localised
    , forLocale
    , readLocalesDir
    , readTemplate
    , readText

      -- * Rendering templates
    , renderText
    , renderHtml

      -- * Re-exports
    , Template
    , template
    ) where

import Brig.Types (Locale (..), parseLocale, locToText)
import Control.Applicative
import Control.Exception (catchJust)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Data.Text.Template (Template, template)
import System.Directory (getDirectoryContents)
import System.IO.Error (isDoesNotExistError)
import Prelude hiding (readFile)

import qualified Data.ByteString    as BS
import qualified Data.Map.Strict    as Map
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy     as Lazy
import qualified Data.Text.Template as Template
import qualified HTMLEntities.Text  as HTML

-- | Localised templates.
data Localised a = Localised
    { locDefault :: !(Locale, a)
    , locOther   :: !(Map Locale a)
    }

readLocalesDir
    :: Locale             -- ^ Default locale.
    -> FilePath           -- ^ Template directory.
    -> (FilePath -> IO a) -- ^ Handler to load the templates for a locale.
    -> IO (Localised a)
readLocalesDir defLocale dir load = do
    def <- load (fullPath defLocaleDir)
    Localised (defLocale, def) <$> do
        ls <- filter eligible <$> getDirectoryContents dir
        Map.fromList . zip (map readLocale ls) <$> mapM (load . fullPath) ls
  where
    fullPath :: FilePath -> FilePath
    fullPath d = dir <> "/" <> d

    eligible :: FilePath -> Bool
    eligible l = l /= defLocaleDir && head l /= '.'

    defLocaleDir :: FilePath
    defLocaleDir = unpack (locToText defLocale)

    readLocale :: FilePath -> Locale
    readLocale l = fromMaybe (error ("Invalid locale: " ++ show l))
                 $ parseLocale (pack l)

-- | Lookup a localised item from a 'Localised' structure.
forLocale :: Maybe Locale
            -- ^ 'Just' the preferred locale or 'Nothing' for
            -- the default locale.
          -> Localised a
            -- ^ The 'Localised' structure.
          -> (Locale, a)
            -- ^ Pair of the effectively chosen locale and the
            -- associated value.
forLocale pref t = case pref of
    Just  l -> fromMaybe (locDefault t) (select l)
    Nothing -> locDefault t
  where
    select l = let l'   = l { lCountry = Nothing }
                   loc  = Map.lookup l (locOther t)
                   lan  = Map.lookup l' (locOther t)
               in (l,) <$> loc <|> (l',) <$> lan

readTemplate :: FilePath -> IO Template
readTemplate f = template <$> readText f

readFile :: FilePath -> IO Text
readFile f = T.decodeUtf8 <$> BS.readFile f

readText :: FilePath -> IO Text
readText f = catchJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                       (readFile f)
                       (\_ -> error $ "Missing file: '" ++ f)

renderText :: Template -> (Text -> Text) -> Lazy.Text
renderText = Template.render

renderHtml :: Template -> (Text -> Text) -> Lazy.Text
renderHtml tpl f = renderText tpl (HTML.text . f)

