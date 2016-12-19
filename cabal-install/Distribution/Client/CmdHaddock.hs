{-# LANGUAGE NamedFieldPuns #-}

-- | cabal-install CLI command: haddock
--
module Distribution.Client.CmdHaddock (
    haddockCommand,
    haddockAction,
  ) where

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectConfig
import Distribution.Client.BuildTarget
         ( readUserBuildTargets )

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..), ConfigExFlags, InstallFlags )
import qualified Distribution.Client.Setup as Client
import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives )
import Distribution.Simple.Setup
         ( HaddockFlags(..), fromFlagOrDefault, fromFlag )
import Distribution.Simple.Utils
         ( wrapText )
import Distribution.Verbosity
         ( normal )

import Control.Monad (unless, void)

haddockCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
haddockCommand = Client.installCommand {
  commandName         = "new-haddock",
  commandSynopsis     = "Build Haddock documentation for the current project",
  commandUsage        = usageAlternatives "new-haddock" [ "[FLAGS] TARGET" ],
  commandDescription  = Just $ \_ -> wrapText $
        "Build Haddock documentation for a Nix-local build project.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " new-haddock cname"
     ++ "    Build documentation for the component named cname\n"
     ++ "  " ++ pname ++ " new-haddock pkgname:cname"
     ++ "    Build documentation for the component named cname in pkgname\n"
   }

-- | The @haddock@ command is TODO.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
--
haddockAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
                 -> [String] -> GlobalFlags -> IO ()
haddockAction (configFlags, configExFlags, installFlags, haddockFlags)
                targetStrings globalFlags = do

    userTargets <- readUserBuildTargets targetStrings

    buildCtx <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning = \_ _ _ -> return (),
          hookSelectPlanSubset = \_ elaboratedPlan -> do
              -- When we interpret the targets on the command line, interpret them as
              -- haddock targets
            targets <- either reportHaddockTargetProblems return
                   =<< resolveTargets
                         (selectPackageTargets haddockFlags)
                         selectComponentTarget
                         TargetProblemCommon
                         elaboratedPlan
                         userTargets
            let elaboratedPlan' = pruneInstallPlanToTargets
                                    TargetActionHaddock
                                    targets
                                    elaboratedPlan
            return elaboratedPlan'
        }

    --TODO: Hmm, but we don't have any targets. Currently this prints what we
    -- would build if we were to build everything. Could pick implicit target like "."
    --TODO: should we say what's in the project (+deps) as a whole?
    printPlan
      verbosity
      buildCtx {
        buildSettings = (buildSettings buildCtx) {
          buildSettingDryRun = True
        }
      }

    unless (buildSettingDryRun (buildSettings buildCtx)) $ void $
      runProjectBuildPhase
        verbosity
        buildCtx
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

-- For haddock: select all buildable libraries, and if the --executables flag
-- is on select all the buildable exes. Do similarly for test-suites,
-- benchmarks and foreign libs.
--
-- There are no failure cases, if there's none of any class, we skip it.
--
selectPackageTargets  :: HaddockFlags -> BuildTarget PackageId
                      -> [AvailableTarget k] -> Either HaddockTargetProblem [k]
selectPackageTargets haddockFlags _bt ts =
    Right [ k | AvailableTarget {
                  availableTargetStatus        = TargetBuildable k _,
                  availableTargetComponentName = cname
                } <- ts
              , isRequested cname ]
  where
    isRequested CLibName      = True
    isRequested CSubLibName{} = True --TODO: unclear if this should be always on
    isRequested CFLibName{}   = fromFlag (haddockForeignLibs haddockFlags)
    isRequested CExeName{}    = fromFlag (haddockExecutables haddockFlags)
    isRequested CTestName{}   = fromFlag (haddockTestSuites  haddockFlags)
    isRequested CBenchName{}  = fromFlag (haddockBenchmarks  haddockFlags)


-- For checking an individual component target, for build there's no
-- additional checks we need beyond the basic ones.
--
selectComponentTarget :: BuildTarget PackageId
                      -> AvailableTarget k -> Either HaddockTargetProblem k
selectComponentTarget bt =
    either (Left . TargetProblemCommon) Right
  . selectComponentTargetBasic bt

data HaddockTargetProblem =
     TargetPackageNoBuildableLibs
   | TargetPackageNoBuildableExes
   | TargetPackageNoEnabledTargets
   | TargetPackageNoTargets
   | TargetProblemCommon TargetProblem
  deriving Show

reportHaddockTargetProblems :: [HaddockTargetProblem] -> IO a
reportHaddockTargetProblems = fail . show
