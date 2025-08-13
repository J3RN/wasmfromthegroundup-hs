module WasmFromTheGroundUp.CH04.Wafer (main, compile) where

import           Data.ByteString.Builder        (Builder)
import           Data.Int                       (Int32)
import           Text.Parsec                    (runParser)
import           Text.Parsec.Language           (emptyDef)
import           Text.Parsec.String             (Parser)
import           Text.Parsec.Token              (LanguageDef, TokenParser,
                                                 integer, makeTokenParser)
import           WasmFromTheGroundUp.CH01.Nop   hiding (main)
import           WasmFromTheGroundUp.CH03.Wafer hiding (compile, main)

main = _

compile = _
