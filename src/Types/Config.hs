{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Config where

import qualified Data.Aeson as JSON
import Data.Set (Set)
import GHC.Generics

-- which commit hash of nixpkgs are we pinned to?
newtype Rev = Rev String
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON)

-- what is the hash of that commit?
newtype Sha256 = Sha256 String
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON)

-- what is the name of our project?
newtype ProjectName = ProjectName String
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON)

-- a dependency we have
newtype Dependency = Dependency String
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON)

-- where to output nix derivation
newtype ShellPath = ShellPath String
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON)

-- our config file in the root of the project
data Config
  = Config
      { rev :: Rev,
        sha256 :: Sha256,
        name :: ProjectName,
        inputs :: Set Dependency,
        nixShellPath :: ShellPath
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data ConfigError
  = CouldNotLoadConfig
