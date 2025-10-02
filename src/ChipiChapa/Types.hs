{-# LANGUAGE TemplateHaskell #-}

module ChipiChapa.Types where

import Control.Lens
import Control.Monad.State
import Data.Vector
import Data.Word

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
  | None
  deriving (Show)

data Chip8 = Chip8
  { _memory :: Vector Word8
  , _registers :: Vector Word8
  , _stack :: [Address]
  , _pointer :: Address
  , _dt :: Int
  , _i :: Int
  }

makeLenses ''Chip8

type ChipIO = StateT Chip8 IO
