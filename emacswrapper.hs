{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative
import Control.Exception
import System.Directory
import System.Environment
import System.FilePath
import System.Exit
import System.Process

#if WINDOWS
import System.Win32.Process (ProcessId)
import Win32Utils
#else
import UnixUtils
#endif

homeKey :: String
homeKey = "HOME"

profileKey :: String
profileKey = "USERPROFILE"

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
    (_,_,_,ph) <- case mdir of
        Just dir -> createProcess $ emacscli (dir </> cmdEmacsclient) args envs
        Nothing  -> do
            mcmd <- findRunemacs
            case mcmd of
                Nothing  -> error $ "Could not find " ++ cmdRunemacs
                Just cmd -> createProcess $ emacs cmd args envs
    code <- waitForProcess ph
    case code of
        ExitSuccess -> return ()
        _           -> exitWith code

cp :: FilePath -> [String] -> [(String, String)] -> CreateProcess
cp exe args envs = CreateProcess
    { cmdspec = RawCommand exe args
    , cwd = Nothing
    , env = Just envs
    , std_in = Inherit
    , std_out = Inherit
    , std_err = Inherit
    , close_fds = True
    , create_group = False
    , delegate_ctlc = False
    }

emacs :: FilePath -> [String] -> [(String, String)] -> CreateProcess
emacs cmd args = cp cmd (["-f", "server-start"] ++ args)

emacscli :: FilePath -> [String] -> [(String, String)] -> CreateProcess
emacscli cmd args = cp cmd ("-n" : if null args
                                   then ["-e", "(raise-frame)"]
                                   else args)

-- | Find full path of runemacs.exe.
findRunemacs :: IO (Maybe FilePath)
findRunemacs = liftA2 (<|>)
                      (findCommandByCurrentProcess cmdRunemacs)
                      (findCommandFromPATH cmdRunemacs)

getHomeEnv :: IO (FilePath, [(String, String)])
getHomeEnv = do
    envs <- getEnvironment
    case lookup profileKey envs <|> lookup homeKey envs of
        Just path ->
            return (path, (homeKey, path) : filter ((/= homeKey) . fst) envs)
        Nothing   -> do
            path <- getHomeDirectory
            return (path, (homeKey, path) : envs)
