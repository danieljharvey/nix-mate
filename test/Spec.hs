import Data.Either
import NixMate.Actions.Config (defaultConfig, loadConfig)
import NixMate.Actions.Docker
import NixMate.Actions.Tags (parseTags)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "tmux-mate" $ do
    it "Parses tags" $ do
      resp <- readFile "./test/data/git-tags.txt"
      let tags = parseTags resp
      length tags `shouldBe` 6
    it "Parses config" $ do
      cfg <- loadConfig "./test/data/nix-mate.json"
      cfg `shouldSatisfy` isRight
    it "Generates Docker derivation" $ do
      docker <- readFile "./test/data/docker-derivation.nix"
      let generated = createDocker defaultConfig
      generated `shouldBe` docker
