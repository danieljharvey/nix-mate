module Actions.Add where

import qualified Actions.Config as Actions
import qualified Actions.Test as Actions
import Data.Coerce
import qualified Data.Set as S
import Types.Config
import Types.Search

type Path = String

addToConfig :: Dependency -> Config -> Config
addToConfig depName cfg =
  cfg {inputs = newPackages}
  where
    newPackages =
      S.insert depName (inputs cfg)

addPackage :: Config -> Path -> Dependency -> IO ()
addPackage cfg path depName = do
  let newConfig = addToConfig depName cfg
  test <- Actions.testDerivation newConfig
  case test of
    Right _ -> do
      Actions.saveConfig path newConfig
      print $ "Added " <> coerce depName <> " to config"
    Left e -> do
      print e
      print $ "Could not find package '" <> coerce depName <> "'"
      pure ()
