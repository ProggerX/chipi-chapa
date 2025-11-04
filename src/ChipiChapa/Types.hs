{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ChipiChapa.Types (
  module ChipiChapa.Types.Opcodes,
  module ChipiChapa.Types.Font,
  module ChipiChapa.Types.Keyboard,
  module ChipiChapa.Types,
) where

import ChipiChapa.Types.Font
import ChipiChapa.Types.Opcodes
import ChipiChapa.Types.Keyboard

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
  , _trace :: Bool
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

beepF :: BS.ByteString
beepF = $(embedFileRelative "beep.wav")
