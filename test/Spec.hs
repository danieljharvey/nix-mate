import Actions.Tags (parseTags)

loadGitResponse :: IO String
loadGitResponse = readFile "./test/data/git-tags.txt"

-- | forgive me the cheapest test suite in the world
main :: IO ()
main = do
  resp <- loadGitResponse
  let tags = parseTags resp
  if length tags == 6
    then pure ()
    else error "Did not find the correct number of tags"
