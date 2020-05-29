module Actions.Config where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Types.Config

type Path = String

loadConfig :: Path -> IO (Maybe Config)
loadConfig path = do
  lbs <- LBS.readFile path
  pure (JSON.decode lbs)

saveConfig :: Path -> Config -> IO ()
saveConfig path cfg = do
  let str = JSON.encode cfg
  LBS.writeFile path str

defaultConfig :: Config
defaultConfig =
  Config
    (Rev "5272327b81ed355bbed5659b8d303cf2979b6953")
    (Sha256 "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq")
    (ProjectName "nix-mate")
    [Dependency "hello"]

init :: IO Config
init = do
  saveConfig "./nix-mate.json" defaultConfig
  pure defaultConfig
