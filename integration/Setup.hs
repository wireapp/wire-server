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
import Language.Haskell.Exts
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

stripHaddock :: String -> String
stripHaddock = \case
  ' ' : '|' : ' ' : xs -> xs
  ' ' : '|' : xs -> xs
  ' ' : xs -> xs
  xs -> xs

collectSummaryAndFullDesc :: [Comment] -> (String, String)
collectSummaryAndFullDesc comments =
  let commentLines = map (\(Comment _ _ s) -> stripHaddock s) comments
   in case uncons commentLines of
        Nothing -> ("", "")
        Just (summary, _) -> (summary, intercalate "\n" commentLines)

getTypeSig :: Decl l -> Maybe (l, [Name l], Type l)
getTypeSig = \case
  TypeSig l names type_ -> Just (l, names, type_)
  _ -> Nothing

collectTestsInModule :: FilePath -> IO [(String, String, String)]
collectTestsInModule fn = do
  s <- readFile fn
  let result = parseFileContentsWithComments defaultParseMode s
  module_ <- case result of
    ParseOk res -> pure (associateHaddock res)
    ParseFailed _sloc msg -> error ("setup: Parsing failed: " <> msg)
  decls <- case module_ of
    Module _ _head _pragmas _imports decls -> pure decls
    _ -> error "setup: Expecting regular module"
  pure $ flip mapMaybe decls $ \decl -> do
    ((_, comments), names, _type) <- getTypeSig decl
    case names of
      ((Ident _ n) : _) ->
        case n of
          ('t' : 'e' : 's' : 't' : _) ->
            let (summary, full) = collectSummaryAndFullDesc comments
             in Just (n, summary, full)
          _ -> Nothing
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
      let cname = CExeName (mkUnqualComponentName "integration-main")
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
