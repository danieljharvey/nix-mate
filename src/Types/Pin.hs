module Types.Pin where

import Types.Config

data PinError
  = CouldNotUseNewRevision
  | CouldNotCalculateSha Rev
  deriving (Eq, Ord, Show)
