import qualified ApiSerializationSpec
import qualified ApiInteractionSpec
import Test.Hspec

main :: IO ()
main = hspec $ do 
  ApiSerializationSpec.spec()
  ApiInteractionSpec.spec
