{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types.Tags (Tag (..), TagName, mkTagName, getTagName) where

import qualified Data.Char as Char
import Data.List (isInfixOf)
import Types.Config (Rev (..))

newtype TagName = TagName String
  deriving newtype (Eq, Ord, Show)

dropS :: String
dropS = "refs/tags/"

mkTagName :: String -> Maybe TagName
mkTagName s =
  let clean = drop (length dropS) s
   in if not (null clean)
        && not ("{}" `isInfixOf` clean)
        && not ("backups" `isInfixOf` clean)
        && not ("alpha" `isInfixOf` clean)
        && not ("beta" `isInfixOf` clean)
        && matchesPred (\a -> Char.isDigit a || a == '.') clean
        then Just (TagName clean)
        else Nothing

matchesPred :: (Char -> Bool) -> String -> Bool
matchesPred f s = length (filter f s) == length s

getTagName :: TagName -> String
getTagName (TagName s) = s

data Tag = Tag
  { tTagName :: TagName,
    tRev :: Rev
  }
  deriving (Eq, Ord, Show)
