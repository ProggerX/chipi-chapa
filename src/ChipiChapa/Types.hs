{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ChipiChapa.Types (
  module ChipiChapa.Types.Opcodes,
  module ChipiChapa.Types.Font,
  module ChipiChapa.Types,
) where

import ChipiChapa.Types.Font
import ChipiChapa.Types.Opcodes

import Control.Lens
import Control.Monad.State
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFileRelative)
import Data.Vector
import Data.Word
import Raylib.Types

data Chip8 = Chip8
  { _memory :: Vector Word8
  , _registers :: Vector Word8
  , _display :: Vector Word32
  , _breakpoints :: Vector Bool
  , _stack :: [Address]
  , _pointer :: Address
  , _dt :: Int
  , _st :: Int
  , _iReg :: Int
  , _debug :: Bool
  , _halted :: Halted
  , _speed :: Int
  }

data Halted
  = AtBreakpoint
  | Waiting Reg
  | Paused
  | Working

data GUI = GUI
  { _memVAddr :: Address
  , _memVCursor :: Address
  , _beep :: Sound
  }

makeLenses ''Chip8
makeLenses ''GUI

type ChipIO = StateT Chip8 IO
type AppM = StateT GUI ChipIO

(@) :: (Functor f, Ixed t) => ((t -> f t) -> c) -> Index t -> (IxValue t -> f (IxValue t)) -> c
x @ y = x . singular (ix y)

key :: Word8 -> KeyboardKey
key = \case
  0x1 -> KeyOne
  0x2 -> KeyTwo
  0x3 -> KeyThree
  0xC -> KeyFour
  0x4 -> KeyQ
  0x5 -> KeyW
  0x6 -> KeyE
  0xD -> KeyR
  0x7 -> KeyA
  0x8 -> KeyS
  0x9 -> KeyD
  0xE -> KeyF
  0xA -> KeyZ
  0x0 -> KeyX
  0xB -> KeyC
  0xF -> KeyV
  _ -> undefined

key' :: KeyboardKey -> Word8
key' = \case
  KeyOne -> 0x1
  KeyTwo -> 0x2
  KeyThree -> 0x3
  KeyFour -> 0xC
  KeyQ -> 0x4
  KeyW -> 0x5
  KeyE -> 0x6
  KeyR -> 0xD
  KeyA -> 0x7
  KeyS -> 0x8
  KeyD -> 0x9
  KeyF -> 0xE
  KeyZ -> 0xA
  KeyX -> 0x0
  KeyC -> 0xB
  KeyV -> 0xF
  _ -> 0x0

keys :: [KeyboardKey]
keys =
  [ KeyOne
  , KeyTwo
  , KeyThree
  , KeyFour
  , KeyQ
  , KeyW
  , KeyE
  , KeyR
  , KeyA
  , KeyS
  , KeyD
  , KeyF
  , KeyZ
  , KeyX
  , KeyC
  , KeyV
  ]

beepF :: BS.ByteString
beepF = $(embedFileRelative "beep.wav")
