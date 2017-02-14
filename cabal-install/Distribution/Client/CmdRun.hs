-------------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Exec
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'new-run' command for building and running an
-- executable supplied by a project.
-------------------------------------------------------------------------------

module Distribution.Client.CmdRun where

import Distribution.Client.InstallPlan
  ( GenericPlanPackage(..)
  , toGraph
  )
import Distribution.Client.ProjectConfig.Types
  ( BuildTimeSettings(buildSettingOnlyDeps)
  )
import Distribution.Client.ProjectOrchestration
  ( PreBuildHooks(..)
  , distDirLayout
  , elaboratedShared
  , printPlan
  , runProjectPreBuildPhase
  , runProjectBuildPhase
  )
import Distribution.Client.ProjectPlanning
  ( ComponentTarget(ComponentTarget)
  , ElaboratedConfiguredPackage(..)
  , ElaboratedInstallPlan
  , PackageTarget(..)
  , SubComponentTarget(WholeComponent)
  , binDirectoryFor
  , pruneInstallPlanToTargets
  )
import Distribution.Client.ProjectPlanning.Types
  ( ElaboratedPackageOrComponent(..)
  , ElaboratedComponent(compComponentName)
  )
import Distribution.Client.Setup
  ( ConfigExFlags
  , ConfigFlags(configVerbosity)
  , GlobalFlags
  , InstallFlags
  , installCommand
  )
import Distribution.Package
  ( PackageIdentifier(pkgName)
  , PackageName
  , UnitId
  )
import Distribution.Simple.Command
  ( CommandUI(..)
  )
import Distribution.Simple.Program.Run
  ( runProgramInvocation
  , simpleProgramInvocation
  )
import Distribution.Simple.Setup
  ( HaddockFlags
  , fromFlagOrDefault
  , readPToMaybe
  )
import Distribution.Simple.Utils
  ( die
  , info
  , wrapText
  )
import Distribution.Text
  ( display
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  )
import Distribution.Types.ComponentName
  ( ComponentName(CExeName)
  )
import Distribution.Types.Executable
  ( Executable(exeName)
  )
import Distribution.Types.PackageDescription
  ( PackageDescription(executables, package)
  )
import Distribution.Verbosity
  ( normal
  )

import Control.Monad (when)
import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map as M
import Data.IORef
import Distribution.Compat.ReadP
import System.FilePath

-- | See also 'UserBuildTarget'. This type encapsulates the ways a user can ask
-- for a specific executable. However, unlike 'UserBuildTarget', which is
-- specific to a package but can talk about any component, a
-- 'UserExecutableTarget' is specific to executable components but is selected
-- from among many packages.
data UserExecutableTarget =

    -- | A target specified by a single name. This could be from any package in
    -- the project.
    --
    -- > cabal new-run exe_foo
    -- > cabal new-run exe_foo -- bar
    UserExecutableName String

    -- | A target specified by package and name.
    --
    -- > cabal new-run pkg_foo:exe_bar
  | UserExecutablePackageName String String
  deriving (Eq, Ord, Read, Show)

userExeName :: UserExecutableTarget -> String
userExeName (UserExecutableName exe) = exe
userExeName (UserExecutablePackageName _ exe) = exe

matchesPackageName :: UserExecutableTarget -> PackageName -> Bool
matchesPackageName (UserExecutableName _) _ = True
matchesPackageName (UserExecutablePackageName pkg _) pkg' = pkg == display pkg'

readUserExecutableTarget :: String -> IO UserExecutableTarget
readUserExecutableTarget s = case readPToMaybe parseTarget s of
  Just target -> return target
  Nothing -> die . unlines $
    [ "Unrecognized executable target syntax '" ++ s ++ "'."
    , "Expected [pkg:]exe where exe is the name of an executable in the"
    , "package pkg."
    ]
  where
  parseTarget =  (UserExecutableName <$> token)
             +++ (UserExecutablePackageName <$> token <*> (char ':' *> token))
  token = munch1 (\x -> not (isSpace x) && x /= ':')

runAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
          -> [String] -> GlobalFlags -> IO ()
runAction _ [] _ = die "Please specify an executable to run"
runAction (configFlags, configExFlags, installFlags, haddockFlags)
          (command:args) globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  userTarget <- readUserExecutableTarget command
  -- In the plan creation, we are going to choose a single package as the one
  -- that matched the user's target. It is handy to have all the package
  -- information available to mess with (e.g. for creating a search path for
  -- the executable), but the hook for planning doesn't have a way to report
  -- information back to the ultimate caller. So we'll cheat and report the
  -- information back through an IORef.
  pkgRef <- newIORef (error singleMatchButNoWrite)
  buildCtx <- runProjectPreBuildPhase
    verbosity
    (globalFlags, configFlags, configExFlags, installFlags, haddockFlags)
    PreBuildHooks
      { hookPrePlanning = \_ _ _ -> return ()
      , hookSelectPlanSubset = \settings elaboratedPlan -> do
          when (buildSettingOnlyDeps settings) . die
             $ "The run command does not support '--only-dependencies'. The "
            ++ "selected executable will always be (re)built. If you really "
            ++ "want to use a potentially stale version of the target, you "
            ++ "might try 'exec', which never builds anything."

          -- For each UnitId in the install plan, we identify candidate
          -- executables. We only keep them if both the package name and
          -- executable name match what the user asked for.
          --
          -- In the common case, we expect this to pick out a single UnitId
          -- that provides a single way of building an appropriately-named
          -- executable. In that case we prune our install plan to that UnitId
          -- and PackageTarget and continue on our merry way.
          --
          -- However, multiple packages/components could provide that
          -- executable, or it's possible we don't find the executable anywhere
          -- in the build plan. I suppose in principle it's also possible that
          -- a single package provides an executable in two different ways,
          -- though that's probably a bug if. Anyway it's a good lint to report
          -- an error in all of these cases, even if some seem like they
          -- shouldn't happen.
          let targetsPkgs = matchingExecutableTargets userTarget elaboratedPlan
              targets = fmap (fmap snd) targetsPkgs
          case concat (M.elems targetsPkgs) of
            [] -> die $ "Unknown executable " ++ userExeName userTarget
                     ++ case userTarget of
                          UserExecutablePackageName pkg _
                            -> " in package " ++ pkg
                          _ -> ""
            [(elabPkg, _)] -> do
              info verbosity $ "Selecting " ++ display (elabUnitId elabPkg)
                            ++ " to supply " ++ userExeName userTarget
              writeIORef pkgRef elabPkg
            _ -> die . unlines
              $ "Multiple matching executables found:"
              : undefined -- TODO: better error reporting here

          return (pruneInstallPlanToTargets targets elaboratedPlan)
      }
  printPlan verbosity buildCtx
  _ <- runProjectBuildPhase verbosity buildCtx
  pkg <- readIORef pkgRef
  let exePath = binDirectoryFor (distDirLayout buildCtx)
                                (elaboratedShared buildCtx)
                                pkg
                                (userExeName userTarget)
             </> userExeName userTarget
  runProgramInvocation
    verbosity
    (simpleProgramInvocation exePath args)
  where
  singleMatchButNoWrite
    =  "Build planning succeeded, but did not choose a unique matching "
    ++ "package to provide your executable. This is a bug."

matchingExecutableTargets
  :: UserExecutableTarget
  -> ElaboratedInstallPlan
  -> Map UnitId [(ElaboratedConfiguredPackage, PackageTarget)]
matchingExecutableTargets userExe = fromElaboratedInstallPlan where
  fromElaboratedInstallPlan = M.fromListWith (++) . fromGraph . toGraph
  fromGraph = foldMap fromPlan

  fromPlan (PreExisting _) = []
  fromPlan (Configured pkg) = fromSrcPkg pkg
  fromPlan (Installed pkg) = fromSrcPkg pkg

  fromSrcPkg
    :: ElaboratedConfiguredPackage
    -> [(UnitId, [(ElaboratedConfiguredPackage, PackageTarget)])]
  fromSrcPkg pkg
    | not (matchesPackageName userExe (elabPkgName pkg)) = []
    | otherwise
      = map (\target -> (elabUnitId pkg, [(pkg, target)]))
      $ case elabPkgOrComp pkg of
        -- For packages, all executables always get built, so it's safe to look
        -- at the PackageDescription. Component-level builds build just one
        -- thing, so we have to be more careful there.
        ElabPackage _ -> fromPackageDescription (elabPkgDescription pkg)
        ElabComponent comp -> case compComponentName comp of
          Just (CExeName exe) -> fromUnqualComponentName exe
          _ -> []

  fromPackageDescription :: PackageDescription -> [PackageTarget]
  fromPackageDescription = foldMap fromExecutable . executables

  fromExecutable :: Executable -> [PackageTarget]
  fromExecutable = fromUnqualComponentName . exeName

  fromUnqualComponentName :: UnqualComponentName -> [PackageTarget]
  fromUnqualComponentName cname =
    [ BuildSpecificComponent (ComponentTarget (CExeName cname) WholeComponent)
    | userExeName userExe == display cname
    ]

  elabPkgName :: ElaboratedConfiguredPackage -> PackageName
  elabPkgName = pkgName . package . elabPkgDescription

runCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
runCommand = installCommand
  { commandName = "new-run"
  , commandSynopsis = "Build and run an executable provided by the project."
  , commandUsage = \pname ->
    "Usage: " ++ pname ++ " new-run [FLAGS] COMMAND [--] [ARGS]\n"
  , commandDescription = Just $ \_pname -> wrapText $ "TODO"
  , commandNotes = Nothing
  }
