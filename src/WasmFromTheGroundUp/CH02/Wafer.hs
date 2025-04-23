module WasmFromTheGroundUp.CH02.Wafer (compile, Err (..)) where

import           Data.ByteString.Builder      (Builder)
import           Data.Int                     (Int32)
import           Text.Parsec                  (runParser)
import           Text.Parsec.Language         (emptyDef)
import           Text.Parsec.String           (Parser)
import           Text.Parsec.Token            (LanguageDef, TokenParser,
                                               integer, makeTokenParser)
import           WasmFromTheGroundUp.CH01.Nop hiding (main)

newtype Err = Error {_message :: String} deriving (Show, Eq)

main :: Parser Int32
main = number

number :: Parser Int32
number = fromIntegral <$> integer wafer

compile :: String -> Either Err Builder
compile source = case parseResult of
  Right val -> Right (encode (modu val))
  Left e    -> Left (Error (show e))
  where
    parseResult = runParser main () "source" source
    modu val =
      Module
        { _typeSection = TypeSection [FunctionType [] [I32]]
        , _functionSection = FunctionSection [FunctionEntry 0]
        , _exportSection = ExportSection [FunctionExport "main" 0]
        , _codeSection = CodeSection [CodeEntry [] [I32Const val, End]]
        }

-- We convert the waferDef (declarative description of the syntax) to a token
-- parser (includes helpful utilities and consumes whitespace between elements).
wafer :: TokenParser ()
wafer = makeTokenParser waferDef

-- We would customize this to define comment syntax, operators, etc, but we
-- don't need any of that (yet)
waferDef :: LanguageDef ()
waferDef = emptyDef
