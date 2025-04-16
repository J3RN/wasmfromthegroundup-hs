module Main (Main.main) where

import           Data.ByteString.Builder        (writeFile)
import           Prelude                        hiding (writeFile)
import           WasmFromTheGroundUp.CH02.Wafer
-- import           WasmFromTheGroundUp.CH01.Nop

main :: IO ()
main = case  (compile "42") of
  Right code       -> writeFile "out.wasm" code
  Left (Error err) -> error err

-- main = WasmFromTheGroundUp.CH01.Nop.main
