{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Bot.Report.Text
    ( formatReport
    , printReport
    , writeReport
    ) where

import Control.Monad.IO.Class
import Data.List (sort)
import Data.Monoid
import Data.Text.Lazy.Builder as Text
import Data.Text.Lazy.Builder.Int
import Network.Wire.Bot.Report hiding (section)
import System.Console.ANSI

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Lazy      as Lazy
import qualified Data.Text.Lazy.IO   as Text

-- | Render a 'Report' as formatted text that can be written to a file
-- or printed to a console.
formatReport :: Bool   -- ^ Whether to pretty-print for console output.
             -> Report -- ^ The report to format.
             -> Lazy.Text
formatReport pretty r = toLazyText $
       "\n" <> title <> "\n"
    <> foldMap section (reportSections r)
  where
    pp x = if pretty then x else mempty

    title = pp underline <> pp bold
            <> fromText (reportTitle r) <> " Report\n\n" <> pp clear
        <> pp bold
            <> fromString (show (reportDate r)) <> pp clear
        <> "\n"

    section s = pp bold <> fromText (sectionName s) <> "\n" <> pp clear
        <> foldMap metric (sectionMetrics s)
        <> "\n"

    metric (Counter l p) = single l $ decimal  (reportCounter r p)
    metric (Label   l p) = single l $ fromText (reportLabel r p)
    metric (Gauge   l p) = single l $ decimal  (reportGauge r p)
    metric (Buckets l p) = multi  l $ sort $ HashMap.toList (reportBucket r p)

    single k v = "\t" <> fromText k   <> ": " <> value v <> "\n"
    multi  k v = "\t" <> subsection k <> "\n" <> foldMap pair v
    pair (b,n) = "\t" <> decimal b    <> ": " <> value (decimal n) <> "\n"

    subsection k = pp underline <> fromText k <> pp clear

    value v = pp (colour Green) <> v <> pp clear

-- | Print a 'Report' to stdout.
printReport :: MonadIO m => Report -> m ()
printReport = liftIO . Text.putStr . formatReport True

-- | Write a 'Report' to a file.
writeReport :: MonadIO m => FilePath -> Report -> m ()
writeReport f = liftIO . Text.writeFile f . formatReport False

underline :: Text.Builder
underline = fromString $ setSGRCode
    [ SetUnderlining SingleUnderline
    ]

bold :: Text.Builder
bold = fromString $ setSGRCode
    [ SetConsoleIntensity BoldIntensity
    ]

colour :: Color -> Text.Builder
colour c = fromString $ setSGRCode
    [ SetColor Foreground Vivid c
    , SetConsoleIntensity NormalIntensity
    ]

clear :: Text.Builder
clear = fromString $ setSGRCode []
