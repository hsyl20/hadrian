module Rules.Register (configurePackage, registerPackage) where

import Distribution.Version (Version)
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PackageId as Cabal

import Base
import Context
import GHC
import Settings
import Target
import Utilities

import Hadrian.Expression

import qualified System.Directory            as IO
import qualified Hadrian.Haskell.Cabal.Parse as Cabal

getPackageNameFromConfFile :: Monad m => FilePath -> m String
getPackageNameFromConfFile conf
   | takeBaseName conf == "rts" = return "rts"
   | otherwise = case parseCabalName (takeBaseName conf) of
      Left err -> error $ "getPackageNameFromConfFile: couldn't parse " ++ takeBaseName conf ++ ": " ++ err
      Right (name, _) -> return name

parseCabalName :: String -> Either String (String, Version)
parseCabalName = fmap f . Cabal.eitherParsec
  where
    f :: Cabal.PackageId -> (String, Version)
    f pkg_id = (Cabal.unPackageName $ Cabal.pkgName pkg_id, Cabal.pkgVersion pkg_id)

-- | Configure a package and build its @setup-config@ file.
configurePackage :: Context -> Rules ()
configurePackage context@Context {..} = do
    root <- buildRootRules
    root -/- contextDir context -/- "setup-config" %> \_ ->
        Cabal.configurePackage context

-- | Registering a package and initialise the corresponding package database if
-- need be.
registerPackage :: [(Resource, Int)] -> Context -> Rules ()
registerPackage rs context@Context {..} = do
    root <- buildRootRules

    -- Initialise the package database.
    root -/- relativePackageDbPath stage -/- packageDbStamp %> \stamp ->
        writeFileLines stamp []

    -- TODO: Add proper error handling for partial functions.
    -- Register a package.
    root -/- relativePackageDbPath stage -/- "*.conf" %> \conf -> do
        settings <- libPath context <&> (-/- "settings")
        platformConstants <- libPath context <&> (-/- "platformConstants")
        need [settings, platformConstants]
        pkgName <- getPackageNameFromConfFile conf
        let Just pkg = findPackageByName pkgName
        isBoot <- (pkg `notElem`) <$> stagePackages Stage0
        case stage of
            Stage0 | isBoot -> copyConf  rs (context { package = pkg }) conf
            _               -> buildConf rs (context { package = pkg }) conf

buildConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
buildConf _ context@Context {..} _conf = do
    depPkgIds <- cabalDependencies context

    -- Calling 'need' on @setupConfig@, triggers the package configuration.
    setupConfig <- pkgSetupConfigFile context
    need [setupConfig]
    need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) depPkgIds

    ways <- interpretInContext context (getLibraryWays <> if package == rts then getRtsWays else mempty)
    need =<< concatMapM (libraryTargets True) [ context { way = w } | w <- ways ]

    -- We might need some package-db resource to limit read/write, see packageRules.
    path <- buildPath context

    -- Special package cases (these should ideally be rolled into Cabal).
    when (package == rts) $
        -- If Cabal knew about "generated-headers", we could read them from the
        -- 'configuredCabal' information, and just "need" them here.
        need [ path -/- "DerivedConstants.h"
             , path -/- "ghcautoconf.h"
             , path -/- "ghcplatform.h"
             , path -/- "ghcversion.h"
             , path -/- "ffi.h" ]

    when (package == integerGmp) $ need [path -/- "ghc-gmp.h"]

    -- Copy and register the package.
    Cabal.copyPackage context
    Cabal.registerPackage context

copyConf :: [(Resource, Int)] -> Context -> FilePath -> Action ()
copyConf rs context@Context {..} conf = do
    depPkgIds <- fmap stdOutToPkgIds . askWithResources rs $
        target context (GhcPkg Dependencies stage) [pkgName package] []
    need =<< mapM (\pkgId -> packageDbPath stage <&> (-/- pkgId <.> "conf")) depPkgIds
    -- We should unregister if the file exists since @ghc-pkg@ will complain
    -- about existing package: https://github.com/snowleopard/hadrian/issues/543.
    -- Also, we don't always do the unregistration + registration to avoid
    -- repeated work after a full build.
    -- We do not track 'doesFileExist' since we are going to create the file if
    -- it is currently missing. TODO: Is this the right thing to do?
    -- See https://github.com/snowleopard/hadrian/issues/569.
    unlessM (liftIO $ IO.doesFileExist conf) $ do
        buildWithResources rs $
            target context (GhcPkg Unregister stage) [pkgName package] []
        buildWithResources rs $
            target context (GhcPkg Clone stage) [pkgName package] [conf]
  where
    stdOutToPkgIds :: String -> [String]
    stdOutToPkgIds = drop 1 . concatMap words . lines
