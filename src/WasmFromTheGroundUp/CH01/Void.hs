module WasmFromTheGroundUp.CH01.Void (main) where

import           Data.ByteString.Builder

main :: Builder
main = header

header :: Builder
header = magic <> version
  where magic = stringUtf8 "\0asm"
        version = word32LE 1
