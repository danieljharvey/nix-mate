module NixMate.Actions.Remove (removePackage) where

import Data.Coerce
import qualified Data.Set as S
import qualified NixMate.Actions.Config as Actions
import NixMate.Types.Config

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

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

removePackage :: Path -> Dependency -> IO Config
removePackage path depName = do
  cfg <- Actions.loadConfig path
  case hush cfg >>= findInConfig depName of
    Just cfg' -> do
      let newCfg = removeFromConfig depName cfg'
      Actions.saveConfig path newCfg
      print $ "Removed " <> coerce depName <> " from config"
      pure newCfg
    _ -> do
      error "Could not find package in config"
