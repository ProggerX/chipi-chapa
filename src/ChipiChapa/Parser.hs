module ChipiChapa.Parser where

import Control.Monad
import Data.Word
import Text.Parsec
import Text.Parsec.String

import ChipiChapa.Types

parseOpcode :: Parser Opcode
parseOpcode =
  (Goto <$> (char '1' >> addrP))
    <|> (Call <$> (char '2' >> addrP))
    <|> try (string "00EE" >> pure Return)
    <|> try (string "00E0" >> pure DispClear)
    <|> (SkipIfEq <$> (char '3' >> regP) <*> b8P)
    <|> (SkipIfNotEq <$> (char '4' >> regP) <*> b8P)
    <|> (SkipIfREq <$> (char '5' >> regP) <*> (regP <* char '0'))
    <|> (SkipIfRNotEq <$> (char '9' >> regP) <*> (regP <* char '0'))
    <|> (RegSet <$> (char '6' >> regP) <*> b8P)
    <|> (CAdd <$> (char '7' >> regP) <*> b8P)
    <|> try (Move <$> (char '8' >> regP) <*> (regP <* char '0'))
    <|> try (BOr <$> (char '8' >> regP) <*> (regP <* char '1'))
    <|> try (BAnd <$> (char '8' >> regP) <*> (regP <* char '2'))
    <|> try (BXor <$> (char '8' >> regP) <*> (regP <* char '3'))
    <|> try (Add <$> (char '8' >> regP) <*> (regP <* char '4'))
    <|> try (Sub <$> (char '8' >> regP) <*> (regP <* char '5'))
    <|> try (RShift <$> do (char '8' >> regP) >> (regP <* char '6'))
    <|> try (SubFrom <$> (char '8' >> regP) <*> (regP <* char '7'))
    <|> try (LShift <$> do (char '8' >> regP) >> (regP <* char 'E'))
    <|> (SetI <$> (char 'A' >> addrP))
    <|> (JmpV0Plus <$> (char 'B' >> addrP))
    <|> (RandomAnd <$> (char 'C' >> regP) <*> b8P)
    <|> (Draw <$> (char 'D' >> regP) <*> regP <*> regP)
    <|> try (RegToDelay <$> do (char 'F' >> regP) <* string "07")
    <|> try (SetDelay <$> do (char 'F' >> regP) <* string "15")
    <|> try (SkipIfNotPressed <$> do (char 'E' >> regP) <* string "A1")
    <|> try (SkipIfPressed <$> do (char 'E' >> regP) <* string "9E")
    <|> try (WaitForKey <$> do (char 'F' >> regP) <* string "0A")
    <|> try (AddI <$> do (char 'F' >> regP) <* string "1E")
    <|> try (StoreBCD <$> do (char 'F' >> regP) <* string "33")
    <|> try (DumpRegs <$> do (char 'F' >> regP) <* string "55")
    <|> try (LoadRegs <$> do (char 'F' >> regP) <* string "65")
    <|> try (FontSprite <$> do (char 'F' >> regP) <* string "29")
    <|> pure None

hStr :: Parser String -> Parser String
hStr p = ("0x" ++) <$> p

regP :: Parser Int
regP = read @Int <$> hStr ((: []) <$> anyChar)

addrP :: Parser Address
addrP = read @Int <$> hStr (replicateM 3 anyChar)

b8P :: Parser Word8
b8P = read @Word8 <$> hStr (replicateM 2 anyChar)
