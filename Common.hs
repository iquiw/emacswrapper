module Common where

import Control.Monad (unless)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>))
import System.Process (CreateProcess(..), CmdSpec(..), StdStream(..),
                       createProcess, waitForProcess)

data EmacsEnv = EmacsEnv
    { eeHome     :: FilePath
    , eeEnvs     :: [(String, String)]
    , eeArgs     :: [String]
    , eeRunemacs :: String
    , eeEmacscli :: String
    } deriving (Eq, Show)

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

runEmacscli :: EmacsEnv -> FilePath -> IO ()
runEmacscli ee dir = do
    (_, _, _, ph) <- createProcess $
                     emacscli (dir </> eeEmacscli ee) (eeArgs ee) (eeEnvs ee)
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $ exitWith ec
