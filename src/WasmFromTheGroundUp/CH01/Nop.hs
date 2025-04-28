module WasmFromTheGroundUp.CH01.Nop ( Encode (..)
                                    , Module (..)
                                    , TypeSection (..)
                                    , TypeEntry (..)
                                    , Type (..)
                                    , FunctionSection (..)
                                    , FunctionEntry (..)
                                    , ExportSection (..)
                                    , ExportEntry (..)
                                    , CodeSection (..)
                                    , CodeEntry (..)
                                    , Local (..)
                                    , Instruction (..)
                                    , main
                                    , sleb128
                                    , uleb128
                                    ) where

import           Data.Bits
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as B
import           Data.Int                (Int32, Int64)
import           Data.Word               (Word32, Word8)

data Module = Module { _typeSection     :: TypeSection
                     , _functionSection :: FunctionSection
                     , _exportSection   :: ExportSection
                     , _codeSection     :: CodeSection
                     }

--- For each section yet covered, each only consists as a vector of their
--- respective entries

newtype TypeSection = TypeSection { _typeSectionEntries :: [TypeEntry]}
data TypeEntry = FunctionType { _paramTypes :: [Type], _returnTypes :: [Type]}
-- These are all added as part of CH02!
data Type = I32 | I64 | F32 | F64

newtype FunctionSection = FunctionSection { _functionSectionEntries :: [FunctionEntry]}
newtype FunctionEntry = FunctionEntry { _functionIndex :: Word32 }

newtype ExportSection = ExportSection {_exportSectionEntries :: [ExportEntry]}
data ExportEntry = FunctionExport { _name :: String, _elementIndex :: Word32}

newtype CodeSection = CodeSection { _codeSectionEntries :: [CodeEntry]}
data CodeEntry = CodeEntry {_locals :: [Local], _instructions :: [Instruction]}
-- L is also a placeholder as we haven't covered locals and do not know what
-- they consist of as yet.
data Local = L
-- We know only one instruction in CH01
data Instruction = End
                 -- These ones are added in CH02
                 | I32Const Int32
                 | I64Const Int64
                 | F32Const Float
                 | F64Const Double
                 -- Added in CH03
                 | I32Add
                 | I32Sub
                 | I32Mul
                 | I32Div_s -- Signed division

main :: Builder
main = encode m
  where m = Module { _typeSection = TypeSection [FunctionType [] []]
                   , _functionSection = FunctionSection [FunctionEntry 0]
                   , _exportSection = ExportSection [FunctionExport "main" 0]
                   , _codeSection = CodeSection [CodeEntry [] [End]]
                   }

--- Generic section encoding
--- Each section has three parts: section identifier, size, contents
section :: Word8 -> Builder -> Builder
section sectionNum body = encodedSectionNum
                          <> sectionSize
                          <> body
  where encodedSectionNum = word8 sectionNum
        sectionSize = uleb128 (B.length (toLazyByteString body))

--- The Encode typeclass specifies an encode function that converts some type a
--- to a ByteString Builder
class Encode a where
  encode :: a -> Builder

-- Encode lists as WASM vectors
instance (Encode a) => Encode [a] where
  encode l = encodedLength <> encodedElements
    -- We make the assumption that the length of the list/vector fits in a U32
    -- (4,294,967,295 elements).  WebAssembly does not allow for longer vectors.
    where encodedLength = u32 (fromIntegral (length l))
          encodedElements = mconcat (map encode l)

--- Module encoding

instance Encode Module where
  encode (Module ts fs es cs) = preamble
                                <> encode ts
                                <> encode fs
                                <> encode es
                                <> encode cs
    where preamble = magic <> version
          magic = stringUtf8 "\0asm"
          version = word32LE 1

--- Type section encoding

instance Encode TypeSection where
  encode (TypeSection entries) = section 1 (encode entries)

instance Encode TypeEntry where
  -- 0x60 indicates a function type.  There might be room to make this more
  -- clear.
  encode (FunctionType {_paramTypes = pt, _returnTypes = rt}) =
    word8 0x60 <> encode pt <> encode rt

instance Encode Type where
  -- These encodings are specified in CH02
  encode I32 = word8 0x7F
  encode I64 = word8 0x7E
  encode F32 = word8 0x7D
  encode F64 = word8 0x7C

--- Function section encoding

instance Encode FunctionSection where
  encode (FunctionSection entries) = section 3 (encode entries)

instance Encode FunctionEntry where
  encode (FunctionEntry fnIx) = u32 fnIx

--- Export section encoding

instance Encode ExportSection where
  encode (ExportSection entries) = section 7 (encode entries)

instance Encode ExportEntry where
  encode (FunctionExport name fnIx) = nameSize
                                      <> encodedName
                                      <> body
    where nameSize = uleb128 (B.length (toLazyByteString encodedName))
          encodedName = stringUtf8 name
          body = word8 0 <> uleb128 fnIx

--- Code section encoding

instance Encode CodeSection where
  encode (CodeSection entries) = section 10 (encode entries)

-- A code entry is the size in bytes followed by the function code
instance Encode CodeEntry where
  encode (CodeEntry {_locals = l, _instructions = i}) = functionSize <> lazyByteString encodedFunction
    where functionSize = uleb128 (B.length encodedFunction)
          -- The function code is a vector of local declarations followed by the function
          -- body, which is a sequence of instructions terminated by the `end` instruction.
          encodedFunction = toLazyByteString (declareLocals l <> mconcat (map encode i))

-- I *believe* this is correct, but we'll see
declareLocals :: [Local] -> Builder
declareLocals = encode

-- What locals will be is unknown, we don't have to care just yet
instance Encode Local where
  encode L = mempty

instance Encode Instruction where
  encode End          = word8 0x0B
  encode (I32Const i) = word8 0x41 <> encode i
  encode (I64Const i) = word8 0x42 <> encode i
  encode (F32Const f) = word8 0x43 <> encode f
  encode (F64Const f) = word8 0x44 <> encode f
  encode I32Add       = word8 0x6A
  encode I32Sub       = word8 0x6B
  encode I32Mul       = word8 0x6C
  encode I32Div_s     = word8 0x6D

instance Encode Int32 where
  encode = i32

instance Encode Int64 where
  encode = sleb128

instance Encode Float where
  encode = floatLE

instance Encode Double where
  encode = doubleLE

--- LEB128 encodings

-- Word32 represents an unsigned 32-bit integer
u32 :: Word32 -> Builder
u32 = uleb128

-- Theoretically this should be limited to only unsigned Integrals,
-- but no such typeclass exists AFAIK and it feels excessive to define
-- one.
uleb128 :: (Integral a, Bits a) => a -> Builder
uleb128 n = case take7 n of
              (byte,    0) -> word8 (toByte byte)
              (byte, rest) -> word8 (markContinue (toByte byte)) <> uleb128 rest

-- Int32 represents a signed 32-bit integer
i32 :: Int32 -> Builder
i32 = sleb128

-- Similarly, this should be limited to only signed Integrals, but no such
-- typeclass exists AFAIK.
sleb128 :: (Integral a, Bits a) => a -> Builder
sleb128 n = case take7 n of
              -- A negative number that has been totally shifted away is -1
              (byte,   -1) | isNegative byte -> word8 byte
              -- A positive number that has been totally shifted away is 0
              (byte,    0) | isPositive byte -> word8 byte
              (byte, rest)                   -> word8 (markContinue byte) <> sleb128 rest
  -- Bits are 0-indexed, so bit 6 is the 7th bit; i.e. the first bit in a 7-bit
  -- segment.  If this is the last segment, the first bit in this segment
  -- indicates the sign for the entire number.  If the "sign" of this byte does
  -- not match the remainder, another byte will be needed to attest the sign of
  -- the number (e.g. see the test for 64).
  where isNegative = (`testBit` 6)
        isPositive = not . isNegative

-- Receives a number and returns a tuple where the first element is the least
-- significant 7 bits packaged in a byte beginning with 0, and the second is the
-- rest of the number with the least significant 7 bits "shifted off".
take7 :: (Integral a, Bits a) => a -> (Word8, a)
take7 n = (toByte (n .&. 0b1111111), n .>>. 7)

-- Mark the byte as not being the last
markContinue :: Word8 -> Word8
markContinue = (`setBit` 7)

-- This will silently truncate the given integer to a single byte.  Thus, the
-- caller must ensure that the integer being passed can already fit in one byte.
toByte :: Integral a => a -> Word8
toByte = fromIntegral
