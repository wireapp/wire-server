{-# LANGUAGE LambdaCase #-}
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
import Distribution.Simple hiding (Module (..))
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.BuildInfo
import Distribution.Types.Executable
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path
import System.Directory
import System.FilePath
import Prelude

collectTests :: [FilePath] -> IO [(String, String, String, String)]
collectTests roots = concat <$> traverse findAllTests (map (<> "/Test") roots)
  where
    findAllTests :: FilePath -> IO [(String, String, String, String)]
    findAllTests root = do
      paths <- findPaths root
      concat <$> traverse (findModuleTests root) paths

    findModuleTests :: FilePath -> FilePath -> IO [(String, String, String, String)]
    findModuleTests root path = do
      let modl = "Test." <> toModule root path
      tests <- collectTestsInModule path
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
          concat <$> traverse findPaths (map (d </>) entries)
        else pure [d]

contexts :: [a] -> [([a], a)]
contexts = go [] []
  where
    go _ctx res [] = res
    go ctx res (x : xs) = go (x : ctx) ((ctx, x) : res) xs

stripHaddock :: String -> String
stripHaddock = \case
  '-' : '-' : ' ' : '|' : ' ' : xs -> xs
  '-' : '-' : ' ' : '|' : xs -> xs
  '-' : '-' : ' ' : xs -> xs
  '-' : '-' : xs -> xs
  xs -> xs

collectDescription :: [String] -> (String, String)
collectDescription revLines =
  let comments = reverse (map stripHaddock (takeWhile isComment revLines))
   in case uncons comments of
        Nothing -> ("", "")
        Just (summary, _) -> (summary, unlines comments)

isComment :: String -> Bool
isComment ('-' : '-' : _) = True
isComment _ = False

collectTestsInModule :: FilePath -> IO [(String, String, String)]
collectTestsInModule fn = do
  s <- readFile fn
  let xs = contexts (lines s)
  pure $ flip mapMaybe xs $ \(previousLines, line) -> do
    case (words line) of
      (name@('t' : 'e' : 's' : 't' : _) : "::" : _) -> do
        let (summary, fullDesc) = collectDescription previousLines
        pure (name, summary, fullDesc)
      _ -> Nothing

testHooks :: UserHooks -> UserHooks
testHooks hooks =
  hooks
    { buildHook = \p l h f -> generate p l >> buildHook hooks p l h f,
      haddockHook = \p l h f -> generate p l >> haddockHook hooks p l h f,
      replHook = \p l h f args -> generate p l >> replHook hooks p l h f args
    }
  where
    generate p l = do
      let cname = CExeName (mkUnqualComponentName "integration")
      let roots = case lookupComponent p cname of
            Just (CExe exe) -> map getSymbolicPath (hsSourceDirs (buildInfo exe))
            _ -> []
      for_ (Map.lookup cname (componentNameMap l)) $ \compBIs ->
        for_ compBIs $ \compBI -> do
          let dest = autogenComponentModulesDir l compBI </> "RunAllTests.hs"
          tests <- collectTests roots
          let modules = Set.toList (Set.fromList (map (\(m, _, _, _) -> m) tests))
          createDirectoryIfMissing True (takeDirectory dest)
          writeFile
            dest
            ( unlines
                [ "module RunAllTests where",
                  "import App",
                  "import Prelude",
                  unlines (map ("import qualified " <>) modules),
                  "allTests :: [(String, String, String, String, App ())]",
                  "allTests =",
                  "  [",
                  "    " <> intercalate ",\n    " (map (\(m, n, s, f) -> "(" <> (intercalate ", " [show m, show n, show s, show f, (m <> "." <> n)]) <> ")") tests),
                  "  ]"
                ]
            )
          pure ()

main :: IO ()
main = do
  defaultMainWithHooks (testHooks simpleUserHooks)
