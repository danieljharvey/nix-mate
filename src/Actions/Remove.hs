module Actions.Remove (removePackage) where

import qualified Actions.Config as Actions
import qualified Actions.CreateNixFile as Actions
import qualified Actions.Search as Actions
import Data.Coerce
import qualified Data.Set as S
import Types.Config
import Types.Search

type Path = String

removeFromConfig :: Dependency -> Config -> Config
removeFromConfig depName cfg =
  cfg {inputs = newPackages}
  where
    newPackages =
      S.delete depName (inputs cfg)

findInConfig :: Dependency -> Config -> Maybe Config
findInConfig depName cfg =
  if S.member depName (inputs cfg) then pure cfg else Nothing

removePackage :: Path -> Dependency -> IO ()
removePackage path depName = do
  cfg <- Actions.loadConfig path
  case cfg >>= findInConfig depName of
    Just cfg' -> do
      let newCfg = removeFromConfig depName cfg'
      Actions.saveConfig path newCfg
      print $ "Removed " <> coerce depName <> " from config"
    _ -> do
      print "Could not find package in config"
      pure ()