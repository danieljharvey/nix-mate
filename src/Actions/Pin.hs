module Actions.Pin (updateRev) where

import qualified Actions.Config as Actions
import Actions.Search
import Types.Config
import Types.Pin

type Path = String

-- let's pin a new version!
updateRev ::
  Config ->
  Path ->
  Rev ->
  IO (Either PinError Config)
updateRev cfg path revision = do
  sha <- getSha256ForRev revision
  case sha of
    Left e -> pure (Left e)
    Right sha' -> do
      let newConfig = cfg {rev = revision, sha256 = sha'}
      Actions.saveConfig path newConfig
      pure (Right newConfig)

-- runs nix-shell, gets paths, cleans them, returns them
getSha256ForRev :: Rev -> IO (Either PinError Sha256)
getSha256ForRev revision = do
  fetched <- safeShell command ""
  case lastLine fetched of
    Just sha -> pure $ Right (Sha256 sha)
    _ -> pure $ Left $ CouldNotCalculateSha revision
  where
    command =
      unwords
        [ "nix-shell",
          "-p",
          "'<nixpkgs>'",
          "--command",
          "'nix-prefetch-url",
          "--unpack",
          getNixPkgsPath revision <> "'"
        ]
    lastLine str = case reverse $ lines str of
      [a] -> Just a
      _ -> Nothing

getNixPkgsPath :: Rev -> String
getNixPkgsPath (Rev r) =
  mconcat
    [ "https://github.com/NixOS/nixpkgs/archive/",
      r,
      ".tar.gz"
    ]
