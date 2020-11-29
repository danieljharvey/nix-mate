module Actions.Config where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as S
import Types.Config

type Path = String

loadConfig :: Path -> IO (Either ConfigError Config)
loadConfig path = do
  lbs <- LBS.readFile path
  case (JSON.decode lbs) of
    Just a -> pure (Right a)
    _ -> pure (Left CouldNotLoadConfig)

saveConfig :: Path -> Config -> IO ()
saveConfig path cfg = do
  let str = JSON.encode cfg
  LBS.writeFile path str

defaultConfig :: Config
defaultConfig =
  Config
    (Rev "2335e7354f8a9c34d4842aa107af14e268a27f07")
    (Sha256 "1mjbv1j3w7wd2hh660scgqzmc9hscrfm96mqrpv1934iz82smkzy")
    (ProjectName "nix-mate")
    (S.fromList [Dependency "hello"])
    (ShellPath "./shell.nix")

init :: IO Config
init = do
  saveConfig "./nix-mate.json" defaultConfig
  pure defaultConfig
