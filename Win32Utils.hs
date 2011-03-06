{-# LANGUAGE ForeignFunctionInterface #-}
module Win32Utils
    ( findCommandByCurrentProcess
    , findCommandFromPATH
    , getArgsW
    , getProcessPath
    , showMessage
    ) where

import Control.Exception (SomeException, try)
import Data.Bits ((.|.))
import Foreign (Ptr, alloca, allocaArray, nullPtr, peek, peekArray)
import Foreign.C (CWString, peekCWString, peekCWStringLen)
import Graphics.Win32 (messageBox, mB_OK)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getEnv, getProgName)
import System.FilePath ((</>), normalise, splitFileName)
import System.Win32 (DWORD, HANDLE, failWith, getLastError, getModuleFileName,
                     localFree, nullHANDLE)
import System.Win32.Process (ProcessId, openProcess,
                             pROCESS_QUERY_INFORMATION, pROCESS_VM_READ)

-- | character size of max path in unicode API
uMaxPath :: DWORD
uMaxPath = 32767

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
    if b
        then return $ Just path
        else return Nothing

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
                mapM peekNfree  $ tail cwss
  where
    peekNfree x = peekCWString x >>= \y -> localFree x >> return y

-- | Return the executable path of the specified PID.
getProcessPath :: ProcessId -> IO FilePath
getProcessPath pid = do
    h <- openProcess (pROCESS_QUERY_INFORMATION .|. pROCESS_VM_READ) False pid
    allocaArray (fromIntegral uMaxPath) $ \ws -> do
        size <- c_GetModuleFileNameExW h nullPtr ws uMaxPath
        if size == 0
            then getLastError >>= failWith "GetModuleFileNameExW"
            else peekCWStringLen (ws, fromIntegral size)

foreign import stdcall "GetCommandLineW" c_GetCommandLineW :: IO CWString

foreign import stdcall "CommandLineToArgvW" c_CommandLineToArgvW
    :: CWString -> Ptr Int -> IO (Ptr CWString)

foreign import stdcall "GetModuleFileNameExW" c_GetModuleFileNameExW
    :: HANDLE -> HANDLE -> CWString -> DWORD -> IO DWORD
