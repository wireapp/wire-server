{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.String
import Distribution.Simple hiding (Language (..), Module (..))
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.BuildInfo
import Distribution.Types.Library
import Distribution.Types.PackageDescription
import Distribution.Utils.Path
import Language.Haskell.Exts (Comment (..), Decl (TypeSig), Language (..), Module (..), Name (..), ParseMode (..), SrcSpanInfo, associateHaddock, fromParseResult, parseFileWithComments)
import qualified Language.Haskell.Exts as Exts
import System.Directory
import System.FilePath
import Prelude

collectTests :: FilePath -> FilePath -> [FilePath] -> IO [(String, String, String, String)]
collectTests pkgRoot topModule roots =
  concat <$> traverse (findAllTests . (</> topModule)) roots
  where
    findAllTests :: FilePath -> IO [(String, String, String, String)]
    findAllTests root = do
      paths <- findPaths root
      concat <$> traverse (findModuleTests root) (filter (not . noise) paths)

    -- don't touch emacs auto-save or backup files
    noise :: FilePath -> Bool
    noise (last . splitDirectories -> fn) = autosave fn || backup fn
      where
        backup = (== '~') . last
        autosave ('.' : '#' : _) = True
        autosave ('#' : _) = True -- (if you want to make this pattern more strict: there is also a '#' at the end.)
        autosave _ = False

    findModuleTests :: FilePath -> FilePath -> IO [(String, String, String, String)]
    findModuleTests root path = do
      let modl = topModule <> "." <> toModule root path
      tests <- collectTestsInModule pkgRoot path
      pure $ map (\(testName, summary, full) -> (modl, testName, summary, full)) tests

    toModule :: FilePath -> FilePath -> String
    toModule root = map setDot . dropExtension . makeRelative root
      where
        setDot '/' = '.'
        setDot c = c

    findPaths :: FilePath -> IO [FilePath]
    findPaths d = do
      isDir <- doesDirectoryExist d
      if isDir
        then do
          entries <- listDirectory d
          concat <$> traverse (findPaths . (d </>)) entries
        else pure [d]

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

    absolutePath = pkgRoot </> fn

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

testHooks :: UserHooks -> UserHooks
testHooks hooks =
  hooks
    { buildHook = \p l h f -> generate p l >> buildHook hooks p l h f,
      haddockHook = \p l h f -> generate p l >> haddockHook hooks p l h f,
      replHook = \p l h f args -> generate p l >> replHook hooks p l h f args
    }
  where
    generate p l = do
      let cname = CLibName LMainLibName
      let roots = case lookupComponent p cname of
            Just (CLib lib) -> map getSymbolicPath (hsSourceDirs (libBuildInfo lib))
            _ -> []
      for_ (Map.lookup cname (componentNameMap l)) $ \compBIs -> do
        for_ compBIs $ \compBI -> do
          let dest = autogenComponentModulesDir l compBI </> "RunAllTests.hs"
          tests <- collectTests (dataDir p) "Test" roots
          perfTests <- collectTests (dataDir p) "Performance" roots
          let modules = Set.toList (Set.fromList (map (\(m, _, _, _) -> m) (tests <> perfTests)))
              mkYieldTests testList =
                unlines (map (\(m, n, s, f) -> "  yieldTests " <> unwords [show m, show n, show s, show f, m <> "." <> n]) testList)
          createDirectoryIfMissing True (takeDirectory dest)
          writeFile
            dest
            ( unlines
                [ "module RunAllTests where",
                  "import Testlib.PTest",
                  "import Prelude",
                  "import Control.Monad.Trans.Writer",
                  unlines (map ("import qualified " <>) modules),
                  "mkAllTests :: IO [Test]",
                  "mkAllTests = execWriterT $ do",
                  mkYieldTests tests,
                  "mkAllPerfTests :: IO [Test]",
                  "mkAllPerfTests = execWriterT $ do",
                  mkYieldTests perfTests
                ]
            )
          pure ()

main :: IO ()
main = do
  defaultMainWithHooks (testHooks simpleUserHooks)
