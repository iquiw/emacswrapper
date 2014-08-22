module Unix
    ( cmdRunemacs
    , cmdEmacsclient
    , findCommandByCurrentProcess
    , isServerRunning
    , getArgsW
    , getHomeEnv
    , runEmacs
    , showMessage
    ) where

import Control.Applicative
import System.Directory
import System.Environment (getArgs, getEnvironment, getExecutablePath,
                           getProgName)
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (createSession, forkProcess)
import System.Posix.User (getRealUserID)
import System.Process (createProcess)

import Common

cmdRunemacs, cmdEmacsclient :: String
cmdRunemacs = "emacs"
cmdEmacsclient = "emacsclient"

runEmacs :: FilePath -> [String] -> [(String, String)] -> IO ()
runEmacs cmd args envs = do
    _ <- forkProcess $ do
        _ <- createSession
        _ <- createProcess $ emacs cmd args envs
        return ()
    return ()

-- | Display message dialog with the specified string.
showMessage :: String -> IO ()
showMessage s = do
    prog <- getProgName
    hPutStrLn stderr $ prog ++ ":" ++ s

-- | Find command from the directory that the executable of the process is in.
findCommandByCurrentProcess :: String -> IO (Maybe FilePath)
findCommandByCurrentProcess cmd = do
    dir <- takeDirectory <$> getExecutablePath
    let path = dir </> cmd
    b <- doesFileExist path
    return $ if b then Just path else Nothing

-- | Get command line arguments.
getArgsW :: IO [String]
getArgsW = getArgs

-- | Get HOME path and modified environment variables.
getHomeEnv :: IO (FilePath, [(String, String)])
getHomeEnv = (,) <$> getHomeDirectory <*> getEnvironment

-- | Check emacs server is running or not.
-- If it is running, return the directory of the executable.
-- Nothing otherwise.
isServerRunning :: IO (Maybe FilePath)
isServerRunning = do
    uid <- getRealUserID
    b <- doesFileExist $ "/tmp/emacs" ++ show uid </> "server"
    if b
        then (takeDirectory <$>) <$> findExecutable cmdRunemacs
        else return Nothing
