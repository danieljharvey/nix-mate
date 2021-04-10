module NixMate.Types.Add where

import NixMate.Types.Config

data AddError
  = CouldNotFindPackage Dependency
  | AlreadyExistsInPackageSet Dependency
  deriving (Eq, Ord, Show)
