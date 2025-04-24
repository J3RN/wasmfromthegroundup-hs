module WasmFromTheGroundUp.CH01.NopSpec (spec) where

import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Builder      as Builder
import qualified Data.ByteString.Lazy         as ByteString
import           Data.Word                    (Word8)
import           Test.Hspec
import qualified WasmFromTheGroundUp.CH01.Nop as Nop

spec :: Spec
spec = describe "Nop" $ do
  describe "sleb128" $ do
    it "encodes 0" $ do
      Nop.sleb128 (0 :: Int) `shouldBinaryMatch` [0b00000000]

    it "encodes 1" $ do
      Nop.sleb128 (1 :: Int) `shouldBinaryMatch` [0b00000001]

    it "encodes -1" $ do
      Nop.sleb128 (-1 :: Int) `shouldBinaryMatch` [0b01111111]

    it "encodes large positive number" $ do
      Nop.sleb128 (123456789 :: Int) `shouldBinaryMatch` [0b10010101, 0b10011010, 0b11101111, 0b00111010]
      -- Continuation bits ─────────────────────────────────┴───────────┴───────────┘           │
      -- Discontinuation bit ───────────────────────────────────────────────────────────────────┘

    it "encodes a large negative number" $ do
      Nop.sleb128 (-123456789 :: Int) `shouldBinaryMatch` [0b11101011, 0b11100101, 0b10010000, 0b01000101]

    it "encodes postive border cases" $ do
      Nop.sleb128 (63 :: Int) `shouldBinaryMatch` [0b00111111]
      Nop.sleb128 (64 :: Int) `shouldBinaryMatch` [0b11000000, 0b000000000]
      Nop.sleb128 (127 :: Int) `shouldBinaryMatch` [0b11111111, 0b00000000]
      Nop.sleb128 (128 :: Int) `shouldBinaryMatch` [0b10000000, 0b00000001]

    it "encodes negative border cases" $ do
      Nop.sleb128 (-63 :: Int) `shouldBinaryMatch` [0b01000001]
      Nop.sleb128 (-64 :: Int) `shouldBinaryMatch` [0b01000000]
      Nop.sleb128 (-127 :: Int) `shouldBinaryMatch` [0b10000001, 0b01111111]
      Nop.sleb128 (-128 :: Int) `shouldBinaryMatch` [0b10000000, 0b01111111]

  describe "main" $ do
    it "matches the expected binary" $ do
      let exprectedBinary = [0x0, 0x61, 0x73, 0x6d, 0x1, 0x0, 0x0, 0x0, 0x1, 0x4, 0x1, 0x60, 0x0, 0x0, 0x3, 0x2, 0x1, 0x0, 0x7, 0x8, 0x1, 0x4, 0x6D, 0x61, 0x69, 0x6E, 0x0, 0x0, 0xA, 0x4, 0x1, 0x2, 0x0, 0xB]
      Nop.main `shouldBinaryMatch` exprectedBinary

shouldBinaryMatch :: HasCallStack => Builder -> [Word8] -> Expectation
shouldBinaryMatch builder bytes = Builder.toLazyByteString builder `shouldBe` ByteString.pack bytes
