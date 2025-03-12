import           Data.ByteString.Builder
import           Prelude                 hiding (writeFile)

main :: IO ()
main = writeFile "out.wasm" header

header :: Builder
header = magic <> version
  where magic = stringUtf8 "\0asm"
        version = word32LE 1
