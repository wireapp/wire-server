{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | This is duplicate code, see /integration/Setup.hs
module FindTests.StolenFromIntegration (collectTestsInModule) where

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Foldable
import Data.Function
import Data.List
-- import Data.Map qualified as Map
import Data.Maybe
-- import Data.Monoid
-- import Data.Set qualified as Set
import Data.String
-- import Distribution.Simple hiding (Language (..), Module (..))
-- import Distribution.Simple.BuildPaths
-- import Distribution.Simple.LocalBuildInfo
-- import Distribution.Types.BuildInfo
-- import Distribution.Types.Library
-- import Distribution.Types.PackageDescription
-- import Distribution.Utils.Path
import Language.Haskell.Exts (Comment (..), Decl (TypeSig), Language (..), Module (..), Name (..), ParseMode (..), SrcSpanInfo, associateHaddock, fromParseResult, parseFileWithComments)
import Language.Haskell.Exts qualified as Exts
-- import System.Directory
import System.Environment
import System.FilePath hiding ((</>))
import System.FilePath qualified
import Prelude

{-
collectTests :: FilePath -> [FilePath] -> IO [(String, String, String, String)]
collectTests pkgRoot modPaths =
  concat <$> (findAllTests `mapM` modPaths)
  where
    findAllTests :: FilePath -> IO [(String, String, String, String)]
    findAllTests modPath = do
      concat <$> traverse findModuleTests (filter (not . noise) paths)

    -- don't touch emacs auto-save or backup files
    noise :: FilePath -> Bool
    noise (last . splitDirectories -> fn) = autosave fn || backup fn
      where
        backup = (== '~') . last
        autosave ('.' : '#' : _) = True
        autosave ('#' : _) = True -- (if you want to make this pattern more strict: there is also a '#' at the end.)
        autosave _ = False

    findModuleTests :: FilePath -> IO [(String, String, String, String)]
    findModuleTests modPath = do
      let modl = toModule modPath
      tests <- collectTestsInModule pkgRoot path
      pure $ map (\(testName, summary, full) -> (modl, testName, summary, full)) tests

    toModule :: FilePath -> String
    toModule = map setDot . dropExtension
      where
        setDot '/' = '.'
        setDot c = c
-}

stripHaddock :: String -> String
stripHaddock = \case
  ' ' : '|' : ' ' : xs -> xs
  ' ' : '|' : xs -> xs
  ' ' : '^' : ' ' : xs -> xs
  ' ' : '^' : xs -> xs
  ' ' : xs -> xs
  xs -> xs

collectDescription :: [String] -> (String, String)
collectDescription ls =
  case uncons $ stripHaddock <$> ls of
    Nothing -> ("", "")
    Just (summary, rest) -> (summary, unlines (dropWhile null rest))

collectTestsInModule :: FilePath -> FilePath -> IO [(String, String, String)]
collectTestsInModule pkgRoot fn = do
  repoRoot <- lookupEnv "WIRE_SERVER_ROOT" >>= maybe (error "*** $WIRE_SERVER_ROOT is not defined") pure
  let absolutePath = repoRoot <//> pkgRoot <//> fn

  -- associateHaddock requires all comments that we want to stick onto a test
  -- should be in the Haddock style, otherwise they won't make it through the parser.
  res <-
    associateHaddock . fromParseResult
      <$> parseFileWithComments
        -- Haskell2010 is the closest we have to getting haskell-src-exts to
        -- playing nicely with GHC2021. One annoying feature it can't handle is
        -- ImportQualifiedPost, so all of our tests have to use traditional
        -- qualified import syntax.
        (ParseMode fn Haskell2010 extensions False False Nothing False)
        absolutePath
  case res of
    Module _ _ _ _ decs ->
      pure $
        decs >>= \case
          TypeSig _ names _ -> mapMaybe testName names
          _ -> []
    _ -> error "XmlPage and XmlHybrid handling not set up. Please fix me!"
  where
    extractComment :: Comment -> String
    extractComment (Comment _ _ s) = s

    testName :: Name (SrcSpanInfo, [Comment]) -> Maybe (String, String, String)
    testName name =
      let (n', comments) =
            case name of
              Ident (_, cs) n -> (n, extractComment <$> cs)
              Symbol (_, cs) n -> (n, extractComment <$> cs)
       in if "test" `isPrefixOf` n'
            then
              let (summary, rest) = collectDescription comments
               in pure (n', summary, rest)
            else Nothing

    -- All of the haskell-src-exts supported extensions that we are using.
    -- Several that are in the cabal file couldn't be directly copied over,
    -- but they aren't causing trouble at the moment.
    -- ImportQualifiedPost is an important one we use elsewhere in this repo
    -- that we can't use in `integration` as haskell-src-exts doesn't support
    -- it currently.
    extensions =
      [ Exts.EnableExtension Exts.BangPatterns,
        Exts.EnableExtension Exts.BlockArguments,
        Exts.EnableExtension Exts.ConstraintKinds,
        Exts.EnableExtension Exts.DataKinds,
        Exts.EnableExtension Exts.DefaultSignatures,
        Exts.EnableExtension Exts.DeriveFunctor,
        Exts.EnableExtension Exts.DeriveGeneric,
        Exts.EnableExtension Exts.DeriveTraversable,
        Exts.EnableExtension Exts.DerivingStrategies,
        Exts.EnableExtension Exts.DerivingVia,
        Exts.EnableExtension Exts.EmptyCase,
        Exts.EnableExtension Exts.FlexibleContexts,
        Exts.EnableExtension Exts.FlexibleInstances,
        Exts.EnableExtension Exts.FunctionalDependencies,
        Exts.EnableExtension Exts.GADTs,
        Exts.EnableExtension Exts.GeneralizedNewtypeDeriving,
        Exts.EnableExtension Exts.InstanceSigs,
        Exts.EnableExtension Exts.KindSignatures,
        Exts.EnableExtension Exts.LambdaCase,
        Exts.EnableExtension Exts.MultiParamTypeClasses,
        Exts.EnableExtension Exts.MultiWayIf,
        Exts.EnableExtension Exts.NamedFieldPuns,
        Exts.EnableExtension Exts.OverloadedLabels,
        Exts.EnableExtension Exts.PackageImports,
        Exts.EnableExtension Exts.PatternSynonyms,
        Exts.EnableExtension Exts.PolyKinds,
        Exts.EnableExtension Exts.QuasiQuotes,
        Exts.EnableExtension Exts.RankNTypes,
        Exts.EnableExtension Exts.RecordWildCards,
        Exts.EnableExtension Exts.ScopedTypeVariables,
        Exts.EnableExtension Exts.StandaloneDeriving,
        Exts.EnableExtension Exts.TupleSections,
        Exts.EnableExtension Exts.TypeApplications,
        Exts.EnableExtension Exts.TypeFamilies,
        Exts.EnableExtension Exts.TypeFamilyDependencies,
        Exts.EnableExtension Exts.TypeOperators,
        Exts.EnableExtension Exts.UndecidableInstances,
        Exts.EnableExtension Exts.ViewPatterns
      ]

-- | `(</>)` from "System.FilePath" interprets double-/ as "delete everything I said so far".
(<//>) :: FilePath -> FilePath -> FilePath
fp1 <//> fp2
  | last fp1 == '/' = init fp1 <//> fp2
  | head fp2 == '/' = fp1 <//> tail fp2
  | otherwise = fp1 System.FilePath.</> fp2
