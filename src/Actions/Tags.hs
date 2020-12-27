module Actions.Tags (fetchTags, parseTags, displayTag, matchTag, findTagByHash, getRecent) where

import Actions.Search
import Data.Maybe
import Types.Config
import Types.Tags

findIn :: (a -> Bool) -> [a] -> Maybe a
findIn f as = case filter f as of
  (a : _) -> Just a
  _ -> Nothing

findTagByHash :: Rev -> [Tag] -> Maybe Tag
findTagByHash rev' = findIn (\t -> tTagHash t == rev')

getRecent :: [Tag] -> Maybe Tag
getRecent = findIn (const True)

matchTag :: String -> [Tag] -> Maybe Tag
matchTag s tags =
  let tagName = mkTagName (tagsPrefix <> s)
   in findIn (\t -> Just (tTagName t) == tagName) tags

displayTag :: Tag -> String
displayTag (Tag tag _rev') =
  getTagName tag

fetchTags :: IO [Tag]
fetchTags = do
  str <- safeShell gitCommand ""
  pure (parseTags str)

parseTags :: String -> [Tag]
parseTags s = reverse $ catMaybes (parseRow <$> items)
  where
    items = lines s

parseRow :: String -> Maybe Tag
parseRow s = case words s of
  (rev' : tag : _) ->
    Tag
      <$> mkTagName tag
        <*> pure (Rev rev')
  _ -> Nothing

gitCommand :: String
gitCommand =
  unwords
    [ "git",
      "ls-remote",
      "--tags",
      "--sort=v:refname",
      "https://github.com/nixos/nixpkgs/"
    ]
