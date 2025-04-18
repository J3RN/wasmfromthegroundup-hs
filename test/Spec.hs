import qualified WasmFromTheGroundUp.CH01.VoidSpec as VoidSpec
import qualified WasmFromTheGroundUp.CH01.NopSpec as NopSpec
import qualified WasmFromTheGroundUp.CH02.WaferSpec as WaferSpec

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "WasmFromTheGroundUp" $ do
    VoidSpec.spec
    NopSpec.spec
    WaferSpec.spec