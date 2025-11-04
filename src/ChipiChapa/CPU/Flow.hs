{-# LANGUAGE LambdaCase #-}
module ChipiChapa.CPU.Flow where

import ChipiChapa.Types
import Control.Lens
import Control.Monad
import Control.Monad.State

execFlow :: (MonadState Chip8 m, MonadIO m) => Flow -> m ()
execFlow = \case
          Return -> do
            use (stack . singular _head) >>= assign pointer
            stack %= drop 1
          Call nnn -> do
            ptr <- use pointer
            stack %= (ptr :)
            pointer .= nnn
          SkipIfEq x nn -> do
            cur <- use $ registers @ x
            when (cur == nn) $ pointer += 2
          SkipIfREq x y -> do
            vx <- use $ registers @ x
            vy <- use $ registers @ y
            when (vx == vy) $ pointer += 2
          SkipIfNotEq x nn -> do
            cur <- use $ registers @ x
            when (cur /= nn) $ pointer += 2
          SkipIfRNotEq x y -> do
            vx <- use $ registers @ x
            vy <- use $ registers @ y
            when (vx /= vy) $ pointer += 2

          JmpV0Plus nnn -> do
            v0 <- fromIntegral <$> use (registers @ 0)
            pointer .= v0 + nnn

          Goto nnn -> pointer .= nnn
