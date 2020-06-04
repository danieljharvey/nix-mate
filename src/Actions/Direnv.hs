module Actions.Direnv (createDirenvRc) where

import Control.Exception (try)
import Types.Direnv

-- here we create an .envrc which adds our new apps to the $PATH
type Path = String

createDirenvRc :: Path -> IO (Either DirenvError ())
createDirenvRc path = do
  result <- try $ writeFile path "watchFile nix-mate.json/nPATH_add $(nix-mate paths)"
  case (result :: Either IOError ()) of
    Right _ -> pure (Right ())
    Left _ -> pure (Left CouldNotWriteEnvrc)
