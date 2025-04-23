import qualified WasmFromTheGroundUp.CH01.NopSpec   as CH01.NopSpec
import qualified WasmFromTheGroundUp.CH01.VoidSpec  as CH01.VoidSpec
import qualified WasmFromTheGroundUp.CH02.WaferSpec as CH02.WaferSpec
import qualified WasmFromTheGroundUp.CH03.WaferSpec as CH03.WaferSpec

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "WasmFromTheGroundUp" $ do
    CH01.VoidSpec.spec
    CH01.NopSpec.spec
    CH02.WaferSpec.spec
    CH03.WaferSpec.spec
