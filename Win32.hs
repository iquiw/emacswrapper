{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Win32
    ( findCommandByCurrentProcess
    , getEmacsEnv
    , isServerRunning
    , runEmacs
    , showMessage
    ) where

import Control.Applicative
import Control.Monad (unless)
import Control.Exception (SomeException, handle)
import Data.Bits ((.|.))
import Foreign (Ptr, alloca, allocaArray, nullPtr, peek, peekArray)
import Foreign.C (CWString, peekCWString, peekCWStringLen)
import Graphics.Win32 (messageBox, mB_OK)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getEnvironment, getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>), normalise, splitFileName)
import System.Process (createProcess, waitForProcess)
import System.Win32 (DWORD, HANDLE, failWith, getLastError, getModuleFileName,
                     localFree, nullHANDLE)
import System.Win32.Process (ProcessId, openProcess,
                             pROCESS_QUERY_INFORMATION, pROCESS_VM_READ)

import Common

homeKey, profileKey :: String
homeKey = "HOME"
profileKey = "USERPROFILE"

-- | character size of max path in unicode API
uMaxPath :: DWORD
uMaxPath = 32767

cmdEmacs, cmdRunemacs, cmdEmacsclient :: String
cmdEmacs = "emacs.exe"
cmdRunemacs = "runemacs.exe"
cmdEmacsclient = "emacsclientw.exe"

-- | Get platform dependent Emacs environment settings.
getEmacsEnv :: IO EmacsEnv
getEmacsEnv = do
    (home, envs) <- getHomeEnv
    EmacsEnv home envs
        <$> getArgsW
        <*> pure cmdRunemacs
        <*> pure cmdEmacsclient

-- | Run emacs server on the specified Emacs environment.
runEmacs :: EmacsEnv -> FilePath -> IO ()
runEmacs ee cmd = do
    (_, _, _, ph) <- createProcess $ emacs cmd (eeArgs ee) (eeEnvs ee)
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $ exitWith ec

-- | Display message dialog with the specified string.
showMessage :: String -> IO ()
showMessage s = do
    prog <- getProgName
    _ <- messageBox nullHANDLE s prog mB_OK
    return ()

-- | Find command from the directory that the executable of the process is in.
findCommandByCurrentProcess :: String -> IO (Maybe FilePath)
findCommandByCurrentProcess cmd = do
    cp <- getModuleFileName nullPtr
    let (dir, _) = splitFileName cp
        path = normalise $ dir </> cmd
    b <- doesFileExist path
    return $ if b then Just path else Nothing

-- | Get command line arguments as unicode string.
getArgsW :: IO [String]
getArgsW = do
    cws <- c_GetCommandLineW
    alloca $ \np -> do
        cwsp <- c_CommandLineToArgvW cws np
        n <- peek np
        if n == 0
            then return []
            else do
                cwss <- peekArray n cwsp
                args <- mapM peekCWString $ tail cwss
                _ <- localFree cwsp
                return args

-- | Get HOME path and modified environment variables.
getHomeEnv :: IO (FilePath, [(String, String)])
getHomeEnv = do
    envs <- getEnvironment
    case lookup profileKey envs <|> lookup homeKey envs of
        Just path ->
            return (path, (homeKey, path) : filter ((/= homeKey) . fst) envs)
        Nothing   -> do
            path <- getHomeDirectory
            return (path, (homeKey, path) : envs)

-- | Return the executable path of the specified PID.
getProcessPath :: ProcessId -> IO FilePath
getProcessPath pid = do
    h <- openProcess (pROCESS_QUERY_INFORMATION .|. pROCESS_VM_READ) False pid
    allocaArray (fromIntegral uMaxPath) $ \ws -> do
        size <- c_GetModuleFileNameExW h nullPtr ws uMaxPath
        if size == 0
            then getLastError >>= failWith "GetModuleFileNameExW"
            else peekCWStringLen (ws, fromIntegral size)

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
            return $ if exe == cmdEmacs then Just dir else Nothing

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

foreign import CALLCONV "GetCommandLineW" c_GetCommandLineW :: IO CWString

foreign import CALLCONV "CommandLineToArgvW" c_CommandLineToArgvW
    :: CWString -> Ptr Int -> IO (Ptr CWString)

foreign import CALLCONV "GetModuleFileNameExW" c_GetModuleFileNameExW
    :: HANDLE -> HANDLE -> CWString -> DWORD -> IO DWORD
