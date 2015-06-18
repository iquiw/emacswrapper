module Unix
    ( findCommandByCurrentProcess
    , getEmacsEnv
    , isServerRunning
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

-- | Get platform dependent Emacs environment settings.
getEmacsEnv :: IO EmacsEnv
getEmacsEnv = EmacsEnv
              <$> getHomeDirectory
              <*> getEnvironment
              <*> getArgs
              <*> pure cmdRunemacs
              <*> pure cmdEmacsclient

-- | Run emacs server on the specified Emacs environment.
runEmacs :: EmacsEnv -> FilePath -> IO ()
runEmacs ee cmd = do
    _ <- forkProcess $ do
        _ <- createSession
        _ <- createProcess $ emacs cmd (eeArgs ee) (eeEnvs ee)
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

-- | Check emacs server is running or not.
-- If it is running, return the directory of the emacsclient executable.
-- Nothing otherwise.
isServerRunning :: IO (Maybe FilePath)
isServerRunning = do
    uid <- getRealUserID
    b <- doesFileExist $ "/tmp/emacs" ++ show uid </> "server"
    if b
        then (takeDirectory <$>) <$> findExecutable cmdEmacsclient
        else return Nothing
