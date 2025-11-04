module ChipiChapa.Types.Opcodes where

import Data.Word

type Address = Int
type Reg = Int

data Opcode
  = Flow Flow
  | Input Input
  | Math Math
  | Regs Regs
  | RandomAnd Reg Word8
  | GetDelay Reg
  | SetDelay Reg
  | SetSound Reg
  | DispClear
  | FontSprite Reg
  | Draw Reg Reg Int
  | None
  deriving (Show)

data Flow
  = Return
  | Goto Address
  | Call Address
  | SkipIfEq Reg Word8
  | SkipIfNotEq Reg Word8
  | SkipIfREq Reg Reg
  | SkipIfRNotEq Reg Reg
  | JmpV0Plus Address
  deriving (Show)

data Input
  = SkipIfNotPressed Reg
  | SkipIfPressed Reg
  | WaitForKey Reg
  deriving (Show)

data Math
  = CAdd Reg Word8
  | BOr Reg Reg
  | BAnd Reg Reg
  | BXor Reg Reg
  | Add Reg Reg
  | Sub Reg Reg
  | RShift Reg Reg
  | SubFrom Reg Reg
  | LShift Reg Reg
  deriving (Show)

data Regs
  = RegSet Reg Word8
  | Move Reg Reg
  | SetI Address
  | AddI Reg
  | StoreBCD Reg
  | DumpRegs Reg
  | LoadRegs Reg
  deriving (Show)
