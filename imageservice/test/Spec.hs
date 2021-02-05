import           Test.Hspec

import qualified Test.Config
import qualified Test.Data.BKTree
import qualified Test.Database
import qualified Test.Network.Images.Html
import qualified Test.Network.Images.Imgur
import qualified Test.Network.Images.Reddit

main :: IO ()
main = hspec $ do
  Test.Config.spec
  Test.Data.BKTree.spec
  Test.Database.spec
  Test.Network.Images.Html.spec
  Test.Network.Images.Imgur.spec
  Test.Network.Images.Reddit.spec
