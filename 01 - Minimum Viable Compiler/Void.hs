import           Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import           System.IO            (IOMode (WriteMode), withFile)

main :: IO ()
main = withFile "out.wasm" WriteMode (`B.hPut` runPut writeHeader)

writeHeader :: Put
writeHeader = magic >> version
  where magic = putStringUtf8 "\0asm"
        version = putWord32le 1
