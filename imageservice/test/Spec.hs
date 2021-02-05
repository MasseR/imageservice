import           Test.Hspec

import qualified Test.Config
import qualified Test.Data.BKTree
import qualified Test.Database

main :: IO ()
main = hspec $ do
  Test.Config.spec
  Test.Data.BKTree.spec
  Test.Database.spec
