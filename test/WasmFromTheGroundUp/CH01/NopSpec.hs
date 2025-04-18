module WasmFromTheGroundUp.CH01.NopSpec (spec) where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString
import Test.Hspec
import qualified WasmFromTheGroundUp.CH01.Nop as Nop

spec :: Spec
spec = describe "Nop" $ do
  describe "main" $ do
    it "should match the expected binary" $ do
      let exprectedBinary = ByteString.pack [0x0, 0x61, 0x73, 0x6d, 0x1, 0x0, 0x0, 0x0, 0x1, 0x4, 0x1, 0x60, 0x0, 0x0, 0x3, 0x2, 0x1, 0x0, 0x7, 0x8, 0x1, 0x4, 0x6D, 0x61, 0x69, 0x6E, 0x0, 0x0, 0xA, 0x4, 0x1, 0x2, 0x0, 0xB] -- "\0asm\1\0\0\0\1\4\1\96\0\0\3\2\1\0\7\8\1\4main\0\0\10"
      let actualBinary = Builder.toLazyByteString Nop.main
      actualBinary `shouldBe` exprectedBinary