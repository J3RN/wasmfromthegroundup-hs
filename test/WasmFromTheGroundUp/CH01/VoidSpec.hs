module WasmFromTheGroundUp.CH01.VoidSpec (spec) where

import qualified Data.ByteString.Builder       as Builder
import qualified Data.ByteString.Lazy          as ByteString
import           Test.Hspec
import qualified WasmFromTheGroundUp.CH01.Void as Void

spec :: Spec
spec = describe "Void" $ do
  describe "main" $ do
    it "should match the expected binary" $ do
      let exprectedBinary = ByteString.pack [0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00]
      let actualBinary = Builder.toLazyByteString Void.main
      actualBinary `shouldBe` exprectedBinary
