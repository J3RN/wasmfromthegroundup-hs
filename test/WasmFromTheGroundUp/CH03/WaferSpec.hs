module WasmFromTheGroundUp.CH03.WaferSpec (spec) where

import qualified Data.ByteString.Builder        as Builder
import qualified Data.ByteString.Lazy           as ByteString
import           Data.Word                      (Word8)
import           Test.Hspec
import           Text.Parsec                    (ParseError, runParser)
import qualified WasmFromTheGroundUp.CH03.Wafer as Wafer
import           WasmFromTheGroundUp.CH03.Wafer (Expr (..), Function (..),
                                                 Op (..))

spec :: Spec
spec = describe "Wafer" $ do
  describe "main" $ do
    it "handles 42" $ "42" `shouldParseTo` Function (ELiteral 42)

    it "handles 1" $ "1" `shouldParseTo` Function (ELiteral 1)

    it "handles addition" $
      "66 + 99" `shouldParseTo` Function (EBinary (ELiteral 66) Add (ELiteral 99))

    it "handles mixed addition and subtraction" $
      "1 + 2 - 3" `shouldParseTo` Function (EBinary (EBinary (ELiteral 1) Add (ELiteral 2)) Subtract (ELiteral 3))

  describe "compile" $ do
    it "compiles plain integer" $ do
      "42" `shouldCompileTo` [0x0, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7F, 0x03, 0x02, 0x01, 0x00, 0x07, 0x08, 0x01, 0x04, 0x6D, 0x61, 0x69, 0x6E, 0x00, 0x00, 0x0A, 0x06, 0x01, 0x4, 0x00, 0x41, 0x2A, 0x0B]

    it "compiles simple arithmetic" $ do
      "1 + 2 - 3" `shouldCompileTo` [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,  0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7f, 0x03,0x02, 0x01, 0x00, 0x07, 0x08, 0x01, 0x04, 0x6d,  0x61, 0x69, 0x6e, 0x00, 0x00, 0x0a, 0x0c, 0x01,0x0a, 0x00, 0x41, 0x01, 0x41, 0x02, 0x6a, 0x41,  0x03, 0x6b, 0x0b]

-- Helpers

parseWafer :: String -> Either ParseError Function
parseWafer = runParser Wafer.main () "test"

shouldParseTo :: HasCallStack => String -> Function -> Expectation
shouldParseTo code expr =
  let actual = parseWafer code
   in actual `shouldBe` Right expr

shouldCompileTo :: HasCallStack => String -> [Word8] -> Expectation
shouldCompileTo code bytes =
  let expected = ByteString.pack bytes
      actual = Builder.toLazyByteString <$> Wafer.compile code
   in actual `shouldBe` Right expected
