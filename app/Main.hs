module Main (Main.main) where

import           Data.ByteString.Builder        (writeFile)
import           Prelude                        hiding (writeFile)
import           WasmFromTheGroundUp.CH03.Wafer

main :: IO ()
main = case compile "2 * (3 - 4)" of
  Right code       -> writeFile "out.wasm" code
  Left (Error err) -> error err
