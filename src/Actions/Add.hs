module Actions.Add where

import qualified Actions.Config as Actions
import qualified Actions.Test as Actions
import qualified Data.Set as S
import Types.Add
import Types.Config

type Path = String

addToConfig :: Dependency -> Config -> Config
addToConfig depName cfg =
  cfg {inputs = newPackages}
  where
    newPackages =
      S.insert depName (inputs cfg)

addPackage ::
  Config ->
  Path ->
  Dependency ->
  IO (Either AddError Config)
addPackage cfg path depName = do
  let newConfig = addToConfig depName cfg
  test <- Actions.testDerivation newConfig
  case test of
    Right _ -> do
      Actions.saveConfig path newConfig
      pure (Right newConfig)
    Left _ ->
      pure (Left (CouldNotFindPackage depName))
