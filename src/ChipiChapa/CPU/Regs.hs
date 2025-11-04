{-# LANGUAGE LambdaCase #-}

module ChipiChapa.CPU.Regs where

import ChipiChapa.Types
import Control.Lens
import Control.Monad
import Control.Monad.State

execRegs :: (MonadState Chip8 m, MonadIO m) => Regs -> m ()
execRegs = \case
  RegSet x nn -> (registers @ x) .= nn
  Move x y -> use (registers @ y) >>= assign (registers @ x)
  SetI nnn -> iReg .= nnn
  AddI x -> use (registers @ x) >>= (iReg +=) . fromIntegral
  StoreBCD x -> do
    vx <- use $ registers @ x

    i <- use iReg
    memory @ (i + 2) .= vx `mod` 10
    memory @ (i + 1) .= vx `div` 10
    memory @ i .= vx `div` 100
  DumpRegs x -> do
    i <- use iReg
    forM_ [0 .. x] $ \l -> do
      r <- use $ registers @ l
      memory @ (i + l) .= r
  LoadRegs x -> do
    i <- use iReg
    forM_ [0 .. x] $ \l -> do
      r <- use $ memory @ (i + l)
      registers @ l .= r
