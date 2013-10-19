import Control.Monad
import Control.Exception
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.InstallDirs hiding (absoluteInstallDirs)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Cmd
import System.Exit
import System.Directory
import System.FilePath
import System.IO.Temp

filesInDirRec :: FilePath -> IO [FilePath]
filesInDirRec dir =
  do entries <- getDirectoryContents dir
     let filtered = filter (`notElem` [".", ".."]) entries
     paths <- liftM join $ mapM (processEntry dir) filtered
     return paths
  where
    processEntry base fp =
      do isDir <- doesDirectoryExist (base</>fp)
         if isDir then liftM (map (fp</>)) $ filesInDirRec (base</>fp)
                  else return [fp]

extraSrcDirs = ["3rd_party"]
extraObjFiles =
  [ "3rd_party/leveldb/libleveldb.a"
  , "3rd_party/snappy/snappy.o"
  , "3rd_party/snappy/snappy-sinksource.o"
  , "3rd_party/snappy/snappy-c.o"
  , "3rd_party/snappy/snappy-stubs-internal.o"
  ]

failExitCode :: ExitCode -> IO ()
failExitCode ExitSuccess = return ()
failExitCode (ExitFailure i) = fail ("Subcommand failed with exit code " ++ show i)

listObjFiles :: FilePath -> IO [FilePath]
listObjFiles dir =
    liftM (filter (\fp -> takeExtension fp `elem` [".o", ".p_o"]))
          (getDirectoryContents dir)

withDir :: FilePath -> (FilePath -> IO a) -> IO a
withDir dir action =
    bracket (do this <- getCurrentDirectory
                setCurrentDirectory dir
                return this)
            setCurrentDirectory
            action

extractAr :: FilePath -> FilePath -> IO ()
extractAr arFile destDir =
    let arBase = takeBaseName arFile
    in withSystemTempDirectory arBase $ \tmpDir ->
       withDir tmpDir $ \base ->
               do rawSystem "ar" ["x", arFile] >>= failExitCode
                  objFiles <- listObjFiles "."
                  forM_ objFiles $ \obj ->
                      copyFile obj (destDir </> (arBase ++ "__" ++ obj))

concatObjs :: [FilePath] -> FilePath -> IO ()
concatObjs fps outp =
    withSystemTempDirectory "leveldb-haskell" $ \tmpDir ->
    withDir tmpDir $ \base ->
    do let paths = map (base</>) fps
       forM_ paths $ \inp ->
          do let ext = takeExtension inp
             case ext of
                ".o" -> copyFile inp (tmpDir </> takeFileName inp)
                ".a" -> extractAr inp tmpDir
       objFiles <- listObjFiles "."
       let target = base </> outp
       replace <- doesFileExist target
       when replace $ removeFile target
       rawSystem "ar" ("rcs" : target : objFiles) >>= failExitCode

main = defaultMainWithHooks
           (simpleUserHooks { preBuild = makeLevelDb
                            , sDistHook = mySDistHook
                            , postInst = myPostInst
                            })
  where
    origSDistHook = sDistHook simpleUserHooks
    mySDistHook pkgDesc mLocBuildInfo hooks flags =
      do putStrLn "Fetching Snappy and LevelDB source files"
         system "bash get.sh" >>= failExitCode
         let processSrcDir dir =
                 do sub <- filesInDirRec dir
                    return $ map (dir</>) sub
         files <- liftM join $ mapM processSrcDir extraSrcDirs
         let newPkgDesc = pkgDesc {
                   extraSrcFiles = extraSrcFiles pkgDesc ++ files }
         origSDistHook newPkgDesc mLocBuildInfo hooks flags
    makeLevelDb _ _ =
      do system "bash build.sh" >>= failExitCode
         return (Nothing, [])
    myPostInst _ iflags pkgDesc lbi =
      do let installDirTmps = installDirTemplates lbi
             libd = libdir installDirTmps
             instLibDir = libdir $ absoluteInstallDirs pkgDesc lbi
                              (fromFlag $ copyDest defaultCopyFlags)
         entries <- getDirectoryContents instLibDir
         forM_ (filter ((==".a") . takeExtension) entries) $ \fname ->
            do let outp = instLibDir </> fname
               putStrLn $ "Patching " ++ outp
               concatObjs (outp : extraObjFiles) outp
