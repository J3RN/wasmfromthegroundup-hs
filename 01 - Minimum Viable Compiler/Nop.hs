import           Data.Binary.Put
import           Data.Bits            ((.|.))
import qualified Data.ByteString.Lazy as B
import           Data.Int             (Int32)
import           Data.Word            (Word32, Word8)
import           System.IO            (IOMode (WriteMode), withFile)

data Module = Module { _typeSection     :: TypeSection
                     , _functionSection :: FunctionSection
                     , _exportSection   :: ExportSection
                     , _codeSection     :: CodeSection
                     }

--- For each section yet covered, each only consists as a vector of their
--- respective entries

newtype TypeSection = TypeSection { _typeSectionEntries :: [TypeEntry]}
data TypeEntry = FunctionType { _paramTypes :: [Type], _returnTypes :: [Type]}
-- T is a placeholder; we don't have any types to encode yet
data Type = T

newtype FunctionSection = FunctionSection { _functionSectionEntries :: [FunctionEntry]}
newtype FunctionEntry = FunctionEntry { _functionIndex :: Word32 }

newtype ExportSection = ExportSection {_exportSectionEntries :: [ExportEntry]}
data ExportEntry = FunctionExport { _name :: String, _elementIndex :: Word32}

newtype CodeSection = CodeSection { _codeSectionEntries :: [CodeEntry]}
data CodeEntry = CodeEntry {_locals :: [Local], _instructions :: [Instruction]}
-- L is also a placeholder as we haven't covered locals and do not know what
-- they consist of as yet.
data Local = L
-- We know only one instruction so far
data Instruction = End

main :: IO ()
main = withFile "out.wasm" WriteMode (`B.hPut` runPut (encode m))
  where m = Module { _typeSection = TypeSection [FunctionType [] []]
                   , _functionSection = FunctionSection [FunctionEntry 0]
                   , _exportSection = ExportSection [FunctionExport "main" 0]
                   , _codeSection = CodeSection [CodeEntry [] [End]]
                   }

--- Generic section encoding
--- Each section has three parts: section identifier, size, contents
section :: Word8 -> Put -> Put
section sectionNum body = putWord8 sectionNum
                          >> putLazyByteString sectionSize
                          >> putLazyByteString encodedContents
  where sectionSize = uleb128 (B.length encodedContents)
        encodedContents = runPut body

--- The Encode typeclass specifies an encode function that converts some type a
--- to a Binary.Put
class Encode a where
  encode :: a -> Put

-- Encode lists as WASM vectors
instance (Encode a) => Encode [a] where
  -- We make an implicit assumption that the length of the list/vector
  -- fits in a U32 (4,294,967,295 elements)
  encode l = putLazyByteString (u32 (fromIntegral (length l)))
             >> mapM_ encode l

--- Module encoding

instance Encode Module where
  encode (Module ts fs es cs) = preamble
                                >> encode ts
                                >> encode fs
                                >> encode es
                                >> encode cs
    where preamble = magic >> version
          magic = putStringUtf8 "\0asm"
          version = putWord32le 1

--- Type section encoding

instance Encode TypeSection where
  encode (TypeSection entries) = section 1 (encode entries)

instance Encode TypeEntry where
  encode (FunctionType {_paramTypes = pt, _returnTypes = rt}) =
    putWord8 0x60 >> encode pt >> encode rt

instance Encode Type where
  encode T = mempty

--- Function section encoding

instance Encode FunctionSection where
  encode (FunctionSection entries) = section 3 (encode entries)

instance Encode FunctionEntry where
  encode (FunctionEntry fnIx) = putLazyByteString (u32 fnIx)

--- Export section encoding

instance Encode ExportSection where
  encode (ExportSection entries) = section 7 (encode entries)

instance Encode ExportEntry where
  encode (FunctionExport name fnIx) = putLazyByteString nameSize
                                      >> putLazyByteString encodedName
                                      >> body
    where nameSize = uleb128 (B.length encodedName)
          encodedName = runPut (putStringUtf8 name)
          body = putWord8 0 >> putLazyByteString (uleb128 fnIx)

--- Code section encoding

instance Encode CodeSection where
  encode (CodeSection entries) = section 10 (encode entries)

-- A code entry is the size in bytes followed by the function code
instance Encode CodeEntry where
  encode (CodeEntry {_locals = l, _instructions = i}) = putLazyByteString functionSize
                                                        >> putLazyByteString encodedFunction
    where functionSize = uleb128 (B.length encodedFunction)
          -- The function code is a vector of local declarations followed by the function
          -- body, which is a sequence of instructions terminated by the `end` instruction.
          encodedFunction = runPut (declareLocals l >> mapM_ encode i)

-- I *believe* this is correct, but we'll see
declareLocals :: [Local] -> Put
declareLocals = encode

-- Again, what locals will be is unknown, we don't have to care just yet
instance Encode Local where
  encode L = mempty

instance Encode Instruction where
  encode End = putWord8 0x0b

--- LEB128 encodings

-- Word32 represents an unsigned 32-bit integer
u32 :: Word32 -> B.ByteString
u32 = uleb128

-- Theoretically this should be limited to only unsigned Integrals,
-- but no such typeclass exists AFAIK and it feels excessive to define
-- one.
uleb128 :: Integral a => a -> B.ByteString
uleb128 n = B.pack (uleb128' n)
  where uleb128' :: Integral a => a -> [Word8]
        uleb128' n' = case n' `quotRem` 128 of
                        (0, byte)    -> [toByte byte]
                        (rest, byte) -> (toByte byte .|. 0x80) : uleb128' rest
        toByte :: Integral a => a -> Word8
        toByte = fromIntegral

-- Int32 represents a signed 32-bit integer
i32 :: Int32 -> B.ByteString
i32 = ileb128

-- Similarly, this should be limited to only signed Integrals, but no
-- such typeclass exists AFAIK.
ileb128 :: Integral a => a -> B.ByteString
ileb128 n = B.pack [fromIntegral n]
