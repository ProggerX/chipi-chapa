{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ChipiChapa.Types where

import Control.Lens
import Control.Monad.State
import Data.Vector
import Data.Word
import Raylib.Types

type Address = Int
type Reg = Int

data Opcode
  = Return
  | Goto Address
  | Call Address
  | SkipIfEq Reg Word8
  | SkipIfNotEq Reg Word8
  | SkipIfREq Reg Reg
  | SkipIfRNotEq Reg Reg
  | RegSet Reg Word8
  | CAdd Reg Word8
  | Move Reg Reg
  | BOr Reg Reg
  | BAnd Reg Reg
  | BXor Reg Reg
  | Add Reg Reg
  | Sub Reg Reg
  | RShift Reg
  | SubFrom Reg Reg
  | LShift Reg
  | SetI Address
  | JmpV0Plus Address
  | RandomAnd Reg Word8
  | Draw Reg Reg Int
  | GetDelay Reg
  | SetDelay Reg
  | DispClear
  | SkipIfNotPressed Reg
  | SkipIfPressed Reg
  | WaitForKey Reg
  | AddI Reg
  | StoreBCD Reg
  | DumpRegs Reg
  | LoadRegs Reg
  | FontSprite Reg
  | None
  deriving (Show)

data Chip8 = Chip8
  { _memory :: Vector Word8
  , _registers :: Vector Word8
  , _display :: Vector Word32
  , _stack :: [Address]
  , _pointer :: Address
  , _dt :: Int
  , _iReg :: Int
  , _debug :: Bool
  , _halted :: Reg
  , _speed :: Int
  }

makeLenses ''Chip8

type ChipIO = StateT Chip8 IO

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

fontData :: Vector Word8
fontData =
  fromList
    [ 0xF0 -- 0
    , 0x90
    , 0x90
    , 0x90
    , 0xF0
    , 0x20 -- 1
    , 0x60
    , 0x20
    , 0x20
    , 0x70
    , 0xF0 -- 2
    , 0x10
    , 0xF0
    , 0x80
    , 0xF0
    , 0xF0 -- 3
    , 0x10
    , 0xF0
    , 0x10
    , 0xF0
    , 0x90 -- 4
    , 0x90
    , 0xF0
    , 0x10
    , 0x10
    , 0xF0 -- 5
    , 0x80
    , 0xF0
    , 0x10
    , 0xF0
    , 0xF0 -- 6
    , 0x80
    , 0xF0
    , 0x90
    , 0xF0
    , 0xF0 -- 7
    , 0x10
    , 0x20
    , 0x40
    , 0x40
    , 0xF0 -- 8
    , 0x90
    , 0xF0
    , 0x90
    , 0xF0
    , 0xF0 -- 9
    , 0x90
    , 0xF0
    , 0x10
    , 0xF0
    , 0xF0 -- A
    , 0x90
    , 0xF0
    , 0x90
    , 0x90
    , 0xE0 -- B
    , 0x90
    , 0xE0
    , 0x90
    , 0xE0
    , 0xF0 -- C
    , 0x80
    , 0x80
    , 0x80
    , 0xF0
    , 0xE0 -- D
    , 0x90
    , 0x90
    , 0x90
    , 0xE0
    , 0xF0 -- E
    , 0x80
    , 0xF0
    , 0x80
    , 0xF0
    , 0xF0 -- F
    , 0x80
    , 0xF0
    , 0x80
    , 0x80
    ]
