module Common where

import System.Process (CreateProcess(..), CmdSpec(..), StdStream(..))

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
