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
import System.Win32.Process (ProcessId)

import Win32Utils

cmdEmacs, cmdRunemacs, cmdEmacsclientw :: String
cmdEmacs = "emacs.exe"
cmdRunemacs = "runemacs.exe"
cmdEmacsclientw = "emacsclientw.exe"

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
        Just dir -> createProcess $ emacscli (dir </> cmdEmacsclientw) args envs
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
    }

emacs :: FilePath -> [String] -> [(String, String)] -> CreateProcess
emacs cmd args = cp cmd (["-f", "server-start"] ++ args)

emacscli :: FilePath -> [String] -> [(String, String)] -> CreateProcess
emacscli cmd args = cp cmd ("-n" : if null args
                                   then ["-e", "(raise-frame)"]
                                   else args)

-- | Check emacs server is running or not.
-- If it is running, return the directory of the executable.
-- Nothing otherwise.
isServerRunning :: IO (Maybe FilePath)
isServerRunning = do
    mpid <- readPidFromServerFile
    case mpid of
        Nothing  -> return Nothing
        -- handles process not found case.
        Just pid -> handle (\(_ :: SomeException) -> return Nothing) $ do
            path <- getProcessPath pid
            let (dir, exe) = splitFileName path
            if exe == cmdEmacs
                then return $ Just dir
                else return Nothing

-- | Read PID from emacs' server file and returns it if exists.
readPidFromServerFile :: IO (Maybe ProcessId)
readPidFromServerFile = do
    f <- ((</> ".emacs.d\\server\\server") . fst) <$> getHomeEnv
    b <- doesFileExist f
    if b
        then readPid f
        else return Nothing
  where
    readPid f = do
        s <- readFile f
        case words s of
            (_:w:_) -> case reads w of
                [(pid, "")] -> return $ Just pid
                _           -> return Nothing
            _       -> return Nothing

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
