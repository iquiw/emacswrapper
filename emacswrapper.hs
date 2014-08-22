{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Monad (unless)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Common
#if WINDOWS
import Win32
#else
import Unix
#endif

main :: IO ()
main = catch winMain showError
  where
    showError :: SomeException -> IO ()
    showError = showMessage . show

winMain :: IO ()
winMain = do
    args  <- getArgsW
    (_, envs) <- getHomeEnv
    mdir  <- isServerRunning
    case mdir of
        Just dir -> do
            (_, _, _, ph) <- createProcess $
                             emacscli (dir </> cmdEmacsclient) args envs
            ec <- waitForProcess ph
            unless (ec == ExitSuccess) $ exitWith ec
        Nothing  -> do
            mcmd <- findRunemacs
            case mcmd of
                Nothing  -> error $ "Could not find " ++ cmdRunemacs
                Just cmd -> runEmacs cmd args envs

-- | Find full path of runemacs.exe.
findRunemacs :: IO (Maybe FilePath)
findRunemacs = liftA2 (<|>)
                      (findCommandByCurrentProcess cmdRunemacs)
                      (findExecutable cmdRunemacs)
