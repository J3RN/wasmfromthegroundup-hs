module WasmFromTheGroundUp.CH03.Wafer (compile, main, Function (..), Expr (..), Op (..), Err (..)) where

import           Data.ByteString.Builder      (Builder)
import           Data.Functor                 (($>))
import           Data.Int                     (Int32)
import           Text.Parsec                  hiding (Error)
import           Text.Parsec.Language         (emptyDef)
import           Text.Parsec.String           (Parser)
import qualified Text.Parsec.Token            as Tok
import           WasmFromTheGroundUp.CH01.Nop hiding (main)

newtype Err = Error {_message :: String} deriving (Show, Eq)

newtype Function = Function Expr
                 deriving (Show, Eq)
data Expr = ELiteral Int32
          | EBinary Expr Op Expr
          deriving (Show, Eq)
data Op = Add | Subtract | Multiply | Divide
        deriving (Show, Eq)

compile :: String -> Either Err Builder
compile source = case parseResult of
  Right fun -> Right (encode (modu fun))
  Left e    -> Left (Error (show e))
  where parseResult = runParser main () "source" source
        modu fun = Module { _typeSection = TypeSection [FunctionType [] [I32]]
                          , _functionSection = FunctionSection [FunctionEntry 0]
                          , _exportSection = ExportSection [FunctionExport "main" 0]
                          , _codeSection = CodeSection [CodeEntry [] (assemble fun)]
                          }

class Assemble a where
  assemble :: a -> [Instruction]

instance Assemble Function where
  assemble (Function e) = assemble e <> [End]

instance Assemble Expr where
  assemble (ELiteral i)     = [I32Const i]
  assemble (EBinary l op r) = assemble l <> assemble r <> opCode op
    where opCode Add      = [I32Add]
          opCode Subtract = [I32Sub]
          opCode Multiply = [I32Mul]
          opCode Divide   = [I32Div_s]

main :: Parser Function
main = Function <$> termExpr

termExpr :: Parser Expr
termExpr = factorExpr `chainl1` do op <- (symbol "+" $> Add) <|> (symbol "-" $> Subtract)
                                   return (\l r -> EBinary l op r)

-- I went a little above and beyond and made multiplication and division higher
-- precedence
factorExpr :: Parser Expr
factorExpr = unaryExpr `chainl1` do op <- (symbol "*" $> Multiply) <|> (symbol "/" $> Divide)
                                    return (\l r -> EBinary l op r)

unaryExpr :: Parser Expr
unaryExpr = parens termExpr <|> (ELiteral <$> number)

-- Helpers

number :: Parser Int32
number = fromIntegral <$> Tok.integer wafer

symbol :: String -> Parser String
symbol = Tok.symbol wafer

parens :: Parser a -> Parser a
parens = Tok.parens wafer

-- We convert the waferDef (declarative description of the syntax) to a token
-- parser (includes helpful utilities and consumes whitespace between elements).
wafer :: Tok.TokenParser ()
wafer = Tok.makeTokenParser waferDef

-- We would customize this to define comment syntax, operators, etc, but we
-- don't need any of that (yet)
waferDef :: Tok.LanguageDef ()
waferDef = emptyDef
