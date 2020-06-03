module Actions.Direnv (createDirenvRc) where

-- here we create an .envrc which adds our new apps to the $PATH
type Path = String

createDirenvRc :: Path -> IO ()
createDirenvRc path = do
  writeFile path "PATH_add $(nix-mate paths)"
  pure ()
