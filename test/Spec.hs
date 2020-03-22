import           Data
import           Lib
import           Test.Hspec

gwc = gridWithCoords grid

findWordTest :: String -> Maybe String
findWordTest word =
  let cells = findWord word gwc
  in case cells of
       Just cs -> Just $ cells2string cs
       _       -> Nothing

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "should concatenate a new line at the end of each line" $ do
      formatGrid (gridWithCoords ["a", "b", "c"]) `shouldBe` "a\nb\nc\n"

  describe "findWord" $ do
    it "should find words that exist in the grid" $ do
      findWordTest "HASKELL" `shouldBe` Just "HASKELL"
      findWordTest "BASIC" `shouldBe` Just "BASIC"
    it "should not find words that don't exist in the grid" $ do
      findWordTest "SCALA" `shouldBe` Nothing

  describe "findWords" $ do
    it "should find all words in the grid" $ do
      map cells2string (findWords languages gwc) `shouldBe` languages
    it "should not find words that don't exist in the grid" $ do
      map cells2string (findWords ["JAVA", "RHOLANG", "FORTRAN"] gwc) `shouldBe` []
