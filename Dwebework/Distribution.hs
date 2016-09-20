-- |Cabal package distribution-related utilities
module Dwebework.Distribution
  ( setGitVersion
  , preBuildRun
  ) where

import Control.Monad (when)
import Data.Version (Version(..), parseVersion)
import Data.Maybe (maybeToList)
import Distribution.Compat.Environment (getEnvironment)
import Distribution.Package (PackageIdentifier(pkgVersion))
import Distribution.PackageDescription (PackageDescription(package, dataDir), GenericPackageDescription(packageDescription), PackageDescription(package))
import Distribution.Simple (UserHooks(confHook, buildHook))
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Setup (fromFlag, buildVerbosity, buildArgs)
import Distribution.Simple.Utils (rawSystemExitWithEnv)
import Distribution.Verbosity (Verbosity)
import System.Directory (getCurrentDirectory)
import System.FilePath ((<.>), (</>))
import System.IO.Error (catchIOError)
import System.Process (readProcess)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (readMaybe)

run :: Verbosity -> PackageDescription -> LocalBuildInfo -> String -> [String] -> IO ()
run verb desc lbi cmd args = do
  env <- getEnvironment
  cwd <- getCurrentDirectory
  rawSystemExitWithEnv verb (buildDir lbi </> cmd </> cmd <.> exeExtension) args
    $ (pkgPathEnvVar desc "datadir", cwd </> dataDir desc)
    : (pkgPathEnvVar desc "sysconfdir", cwd)
    : env

gitDescribe :: String -> [String] -> IO (Maybe Version)
gitDescribe pfx args = do
  s <- readProcess "git" (["describe", "--long", "--dirty", "--match", pfx ++ "[0-9]*"] ++ args) ""
    `catchIOError` \_ -> return ""
  return $ case [ v | (v, "\n") <- ReadP.readP_to_S (ReadP.string pfx >> parseVersion) s ] of
    [Version n (c:t)] -> Just $ Version (n ++ maybeToList (readMaybe c)) t
    _ -> Nothing

-- |Update the package version according to a git tag.
-- @setGitVersionUserHooks \"prefix\" ARGS@ uses @git describe --long --dirty --match prefix[0-9]* ARGS@
-- and attempts to parse the output as \"prefixVERSION-COUNT-TAGS\".
-- If successful, it uses \"VERSION.COUNT-TAGS\" as the package version.
-- For example, in Setup.hs:
--
-- > main = defaultMainWithHooks $ setGitVersion "v" [] simpleUserHooks
setGitVersion
  :: String -- ^The tag prefix for version tags
  -> [String] -- ^Additional arguments to pass to git describe
  -> UserHooks -> UserHooks
setGitVersion pfx args uhooks = uhooks
  { confHook = \(g, i) f -> do
    let s v = g{ packageDescription = d{ package = p{ pkgVersion = v } } }
        d = packageDescription g
        p = package d
    d' <- maybe g s <$> gitDescribe pfx args
    confHook uhooks (d', i) f
  }

-- |Build and run a program before building any other targets by default.
-- The program must be the name of a configured executable.
-- For example:
--
-- > main = defaultMainWithHooks $ preBuildRun "dbsetupschema" [] simpleUserHooks
preBuildRun
  :: String -- ^The program to run, which must be the name of a configured package executable
  -> [String] -- ^Arguments to pass to the program
  -> UserHooks -> UserHooks
preBuildRun prog progargs uhooks = uhooks
  { buildHook = \desc lbi hooks flag -> do
    let verb = fromFlag $ buildVerbosity flag
        args = buildArgs flag
        build c = buildHook uhooks desc lbi hooks flag{ buildArgs = c }
    when (null args) $ do
      build [prog]
      run verb desc lbi prog progargs
    build args
  }
