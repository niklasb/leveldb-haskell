import Control.Monad
import Distribution.PackageDescription
import Distribution.Simple
import System.Cmd
import System.Directory
import System.FilePath

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

main = defaultMainWithHooks
           (simpleUserHooks { preBuild = makeLevelDb
                            , sDistHook = mySDistHook
                            })
  where
    origSDistHook = sDistHook simpleUserHooks
    mySDistHook pkgDesc mLocBuildInfo hooks flags =
      do putStrLn "Fetching Snappy and LevelDB source files"
         void $ system "bash get.sh"
         let processSrcDir dir =
                 do sub <- filesInDirRec dir
                    return $ map (dir</>) sub
         files <- liftM join $ mapM processSrcDir extraSrcDirs
         let newPkgDesc = pkgDesc {
                   extraSrcFiles = extraSrcFiles pkgDesc ++ files }
         origSDistHook newPkgDesc mLocBuildInfo hooks flags
    makeLevelDb _ _ =
      do void $ system "bash build.sh"
         return (Nothing, [])
