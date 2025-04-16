module WasmFromTheGroundUp.CH02.Wafer (compile, Err (..)) where

import           Data.ByteString.Builder      (Builder)
import           Data.Int                     (Int32)
import           Data.Text                    (Text)
import           Text.Parsec                  (digit, many1, runParser)
import           Text.Parsec.Text             (Parser)
import           WasmFromTheGroundUp.CH01.Nop hiding (main)

data Err = Error { _message :: String }

main :: Parser Int32
main = number

number :: Parser Int32
number = read <$> many1 digit

compile :: Text -> Either Err Builder
compile source = case parseResult of
                   Right val -> Right (encode (m val))
                   Left e    -> Left (Error (show e))
  where parseResult = runParser main () "source" source
        m v = Module { _typeSection = TypeSection [FunctionType [] [I32]]
                     , _functionSection = FunctionSection [FunctionEntry 0]
                     , _exportSection = ExportSection [FunctionExport "main" 0]
                     , _codeSection = CodeSection [CodeEntry [] [I32Const v, End]]
                     }
