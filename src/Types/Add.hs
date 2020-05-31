{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Add where

import Types.Config

data AddError
  = CouldNotFindPackage Dependency
  | AlreadyExistsInPackageSet Dependency
  deriving (Eq, Ord, Show)
