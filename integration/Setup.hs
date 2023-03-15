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
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.BuildInfo
import Distribution.Types.Executable
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path
import System.Directory
import System.FilePath
import Prelude

collectTests :: [FilePath] -> IO [(String, String)]
collectTests roots = concat <$> traverse findAllTests (map (<> "/Test") roots)
  where
    findAllTests :: FilePath -> IO [(String, String)]
    findAllTests root = do
      paths <- findPaths root
      concat <$> traverse (findModuleTests root) paths

    findModuleTests :: FilePath -> FilePath -> IO [(String, String)]
    findModuleTests root path = do
      let modl = "Test." <> toModule root path
      source <- readFile path
      let names = catMaybes (map testName (lines source))
      pure $ map (\n -> (modl, n)) names

    testName :: String -> Maybe String
    testName line = case break (== "::") (words line) of
      ([('t' : 'e' : 's' : 't' : name)], _) -> Just name
      _ -> Nothing

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
          concat <$> traverse findPaths (map (d </>) entries)
        else pure [d]

testHooks :: UserHooks -> UserHooks
testHooks hooks =
  hooks
    { buildHook = \p l h f -> generate p l >> buildHook hooks p l h f,
      haddockHook = \p l h f -> generate p l >> haddockHook hooks p l h f,
      replHook = \p l h f args -> generate p l >> replHook hooks p l h f args
    }
  where
    generate p l = do
      let cname = CExeName (mkUnqualComponentName "integration-main")
      let roots = case lookupComponent p cname of
            Just (CExe exe) -> map getSymbolicPath (hsSourceDirs (buildInfo exe))
            _ -> []
      for_ (Map.lookup cname (componentNameMap l)) $ \compBIs ->
        for_ compBIs $ \compBI -> do
          let dest = autogenComponentModulesDir l compBI </> "RunAllTests.hs"
          tests <- collectTests roots
          let modules = Set.toList (Set.fromList (map fst tests))
          createDirectoryIfMissing True (takeDirectory dest)
          writeFile
            dest
            ( unlines
                [ "module RunAllTests where",
                  "import App",
                  unlines (map ("import qualified " <>) modules),
                  "runAllTests :: App ()",
                  "runAllTests = do",
                  unlines (map (\(m, n) -> "  " <> m <> ".test" <> n) tests)
                ]
            )
          pure ()

main :: IO ()
main = do
  defaultMainWithHooks (testHooks simpleUserHooks)
