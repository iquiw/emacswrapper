{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative
import Control.Exception
import System.FilePath
import System.Exit
import System.Process

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
    (_, _, _, ph) <- case mdir of
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
