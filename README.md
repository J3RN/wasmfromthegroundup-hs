# WebAssembly from the Ground Up - Haskell

:warning:Work in progress!  I am only partway through the book, and will add more ports as I go.:warning:

This is a collection of code from [WebAssembly from the Ground Up] ported from JavaScript and Ohm to Haskell.

- Chapter 1: Minimum Viable Compiler
  - [Void Lang](./src/WasmFromTheGroundUp/CH01/Void.hs)
  - [Nop Lang](./src/WasmFromTheGroundUp/CH01/Nop.hs)
- Chapter 2: 
  - [Wafer](./src/WasmFromTheGroundUp/CH02/Wafer.hs)

## General Notes

- Whereas JavaScript uses `Uint8Array` to represent an array of bytes, this Haskell code uses bytestring builders (`Data.ByteString.Builder`).  The `Data.ByteString.Builder` module provides many helpers that WebAssembly from the Ground Up must implement from scratch for JavaScript (e.g. `stringUtf8`, `word32LE`).
- I'm not a Haskell expert.  If you have a cleaner way to implement something, send me a pull request!
- That said, I'm trying to keep the code fairly understandable to someone with a limited knowledge of Haskell.  I'm using typeclasses, but this is about as advanced as it get (at the moment).  I'm intentionally doing function composition with parentheses (e.g. `()`) instead of the composition operator (`.`) for this approachability reason.
- My Haskell code style is probably bad and/or inconsistent, I'm not using a formatter at the moment, but I'm open to it.


[WebAssembly from the Ground Up]: https://wasmgroundup.com
