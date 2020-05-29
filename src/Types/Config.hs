{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Config where

import qualified Data.Aeson as JSON
import GHC.Generics

{-
{
  "rev": "5272327b81ed355bbed5659b8d303cf2979b6953",
  "sha256": "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq",
  "name": "nix-mate",
  "inputs": ["yarn", "cowsay"]
}

-}

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

-- our config file in the root of the project
data Config
  = Config
      { rev :: Rev,
        sha256 :: Sha256,
        name :: ProjectName,
        inputs :: [Dependency]
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)
