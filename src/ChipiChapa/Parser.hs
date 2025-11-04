module ChipiChapa.Parser where

import Control.Monad
import Data.Word
import Text.Parsec
import Text.Parsec.String

import ChipiChapa.Types

flow, math, regs, input :: [Parser Opcode]
flow =
  map ((Flow <$>) . try) $
    [ (Goto <$> (char '1' >> addrP))
    , (Call <$> (char '2' >> addrP))
    , (string "00EE" >> pure Return)
    , (SkipIfEq <$> (char '3' >> regP) <*> b8P)
    , (SkipIfNotEq <$> (char '4' >> regP) <*> b8P)
    , (SkipIfREq <$> (char '5' >> regP) <*> (regP <* char '0'))
    , (SkipIfRNotEq <$> (char '9' >> regP) <*> (regP <* char '0'))
    , (JmpV0Plus <$> (char 'B' >> addrP))
    ]
math =
  map ((Math <$>) . try) $
    [ (CAdd <$> (char '7' >> regP) <*> b8P)
    , (BOr <$> (char '8' >> regP) <*> (regP <* char '1'))
    , (BAnd <$> (char '8' >> regP) <*> (regP <* char '2'))
    , (BXor <$> (char '8' >> regP) <*> (regP <* char '3'))
    , (Add <$> (char '8' >> regP) <*> (regP <* char '4'))
    , (Sub <$> (char '8' >> regP) <*> (regP <* char '5'))
    , (RShift <$> do (char '8' >> regP) >> (regP <* char '6'))
    , (SubFrom <$> (char '8' >> regP) <*> (regP <* char '7'))
    , (LShift <$> do (char '8' >> regP) >> (regP <* char 'E'))
    ]
regs =
  map ((Regs <$>) . try) $
    [ (RegSet <$> (char '6' >> regP) <*> b8P)
    , (Move <$> (char '8' >> regP) <*> (regP <* char '0'))
    , (SetI <$> (char 'A' >> addrP))
    , (AddI <$> do (char 'F' >> regP) <* string "1E")
    , (StoreBCD <$> do (char 'F' >> regP) <* string "33")
    , (DumpRegs <$> do (char 'F' >> regP) <* string "55")
    , (LoadRegs <$> do (char 'F' >> regP) <* string "65")
    ]
input =
  map ((Input <$>) . try) $
    [ (SkipIfNotPressed <$> do (char 'E' >> regP) <* string "A1")
    , (SkipIfPressed <$> do (char 'E' >> regP) <* string "9E")
    , (WaitForKey <$> do (char 'F' >> regP) <* string "0A")
    ]

opcode :: Parser Opcode
opcode =
  foldr (<|>) (pure None) $
    concat
      [ flow
      , input
      , math
      , regs
      , map
          try
          [ (string "00E0" >> pure DispClear)
          , (RandomAnd <$> (char 'C' >> regP) <*> b8P)
          , (Draw <$> (char 'D' >> regP) <*> regP <*> regP)
          , (GetDelay <$> do (char 'F' >> regP) <* string "07")
          , (SetDelay <$> do (char 'F' >> regP) <* string "15")
          , (SetSound <$> do (char 'F' >> regP) <* string "18")
          , (FontSprite <$> do (char 'F' >> regP) <* string "29")
          ]
      ]

hStr :: Parser String -> Parser String
hStr p = ("0x" ++) <$> p

regP :: Parser Int
regP = read @Int <$> hStr ((: []) <$> anyChar)

addrP :: Parser Address
addrP = read @Int <$> hStr (replicateM 3 anyChar)

b8P :: Parser Word8
b8P = read @Word8 <$> hStr (replicateM 2 anyChar)
