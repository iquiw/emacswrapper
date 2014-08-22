{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative ((<|>), liftA2)
import Control.Exception (SomeException, catch)
import System.Directory (findExecutable)

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
    ee <- getEmacsEnv
    mdir  <- isServerRunning
    case mdir of
        Just dir -> runEmacscli ee dir
        Nothing  -> do
            mcmd <- findRunemacs ee
            case mcmd of
                Nothing  -> error $ "Could not find " ++ (eeRunemacs ee)
                Just cmd -> runEmacs ee cmd

-- | Find full path of runemacs.exe.
findRunemacs :: EmacsEnv -> IO (Maybe FilePath)
findRunemacs ee = liftA2 (<|>)
                      (findCommandByCurrentProcess (eeRunemacs ee))
                      (findExecutable (eeRunemacs ee))
