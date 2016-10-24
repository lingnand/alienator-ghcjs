import Control.Monad
import Data.List
import System.Environment
import System.Directory
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

deployDir :: String
deployDir = "src"

deployJs :: LocalBuildInfo -> IO ()
deployJs info = do
    let files = ["rts", "lib", "out"]
    putStrLn $ "Deploying {" ++ intercalate "," files ++ "}.js..."
    let deployExe [] =  putStrLn "ERROR: path to the binary cannot be obtained"
        deployExe ((CExeName name, _, _):_) = do
            let exeDir = intercalate "/" [buildDir info, name, name++".jsexe"]
            createDirectoryIfMissing False deployDir
            forM_ files $ \f -> do
                let src = exeDir ++ "/" ++ f ++ ".js"
                    dest = deployDir ++ "/" ++ f ++ ".js"
                exists <- doesFileExist src
                if exists then copyFile src dest >> putStrLn (src ++ " copied to " ++ dest)
                          else putStrLn $ "ERROR: " ++ src ++ " does not exist"
        deployExe (_:xs) = deployExe xs
    deployExe $ componentsConfigs info

main = do
    args <- getArgs
    if "build" `elem` args
      -- only trigger deployment when 'build' is specified
      then defaultMainWithHooksArgs simpleUserHooks{ postBuild = \_ _ _ info -> deployJs info } args
      else defaultMainWithHooksArgs simpleUserHooks args
