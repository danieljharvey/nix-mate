module Actions.Add where

import qualified Actions.Config as Actions
import qualified Actions.CreateNixFile as Actions
import qualified Actions.Search as Actions
import Data.Coerce
import Types.Config
import Types.Search

type Path = String

addToConfig :: Dependency -> Config -> Config
addToConfig depName cfg =
  cfg {inputs = newPackages}
  where
    newPackages =
      inputs cfg <> [depName]

findPackage :: Dependency -> IO (Maybe SearchPackage)
findPackage depName = do
  found <- Actions.search depName
  case found of
    Found a -> pure (Just a)
    _ -> pure Nothing

addPackage :: Path -> Dependency -> IO ()
addPackage path depName = do
  found <- findPackage depName
  cfg <- Actions.loadConfig path
  case (,) <$> found <*> cfg of
    Just (_, cfg') -> do
      let newCfg = addToConfig depName cfg'
      Actions.saveConfig path newCfg
      print $ "Added " <> coerce depName <> " to config"
      Actions.createNixFile "./shell.nix" newCfg
    _ -> do
      print "Could not find package"
      pure ()
