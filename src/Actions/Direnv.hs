module Actions.Direnv (createDirenvRc, direnvReload) where

-- here we create an .envrc which adds our new apps to the $PATH
type Path = String

createDirenvRc :: Path -> IO ()
createDirenvRc path = do
  writeFile path "PATH_add $(nix-mate paths)"
  pure ()

-- run a shell action that throws
safeShell :: String -> IO ()
safeShell command = do
  let myShell = (shell command) {cwd = (Just ".")}
  _ <- try (readCreateProcess (myShell) "")
  pure ()

direnvReload :: IO ()
direnvReload = safeShell "direnv reload"
