import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple ( UserHooks(..)
                           , defaultMainWithHooks
                           , simpleUserHooks )
import Distribution.Simple.Program (requireProgram, simpleProgram)
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Simple.Setup (buildVerbosity, fromFlag)
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.System (Arch(..), buildArch)

main ::  IO ()
main = defaultMainWithHooks $ simpleUserHooks
    { hookedPrograms = [windres]
    , preBuild = createRes
    }
  where
    windres = simpleProgram "windres"
    createRes _ flags = do
        let verbosity = fromFlag $ buildVerbosity flags
            arch = if buildArch == X86_64 then "pe-x86-64" else "pe-i386"
            args = [ "-O", "coff"
                   , "-F", arch
                   , "-i", "emacswrapper.rc"
                   , "-o", "dist/emacswrapper.res"
                   ]
        -- The following does not work!
        -- (cp, _) <- requireProgram verbosity windres defaultProgramDb
        -- runProgramInvocation verbosity (programInvocation cp args)

        _ <- requireProgram verbosity windres defaultProgramDb
        rawSystemExit verbosity "windres" args
        return emptyHookedBuildInfo
