{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.CreateNixFile where

newtype Derivation = Derivation String
  deriving newtype (Show, Semigroup)
