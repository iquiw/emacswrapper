{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Win32Utils
    ( cmdRunemacs
    , cmdEmacsclient
    , findCommandByCurrentProcess
    , findCommandFromPATH
    , isServerRunning
    , getArgsW
    , getHomeEnv
    , showMessage
    ) where

import Control.Applicative
import Control.Exception (SomeException, handle, try)
import Data.Bits ((.|.))
import Foreign (Ptr, alloca, allocaArray, nullPtr, peek, peekArray)
import Foreign.C (CWString, peekCWString, peekCWStringLen)
import Graphics.Win32 (messageBox, mB_OK)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.Environment (getEnv, getEnvironment, getProgName)
import System.FilePath ((</>), normalise, splitFileName)
import System.Win32 (DWORD, HANDLE,
                     failWith, getLastError, getModuleFileName, nullHANDLE)
import System.Win32.Process (ProcessId, openProcess,
                             pROCESS_QUERY_INFORMATION, pROCESS_VM_READ)

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

-- | Find command from the current directory and PATH environment variable.
findCommandFromPATH :: String -> IO (Maybe FilePath)
findCommandFromPATH cmd = do
    dir <- getCurrentDirectory
    ep <- try $ getEnv "PATH" :: IO (Either SomeException String)
    let paths = case ep of
            Left _  -> [dir]
            Right p -> dir : sp p
    getValidPath paths
  where
    getValidPath []     = return Nothing
    getValidPath (d:ds) = let p = normalise $ d </> cmd
                          in do
                              b <- doesFileExist p
                              if b
                                  then return $ Just p
                                  else getValidPath ds
    sp s = case break (==';') s of
        (s', [])  -> [s']
        (s', s'') -> s' : sp (tail s'')

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
                mapM peekCWString $ tail cwss

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
