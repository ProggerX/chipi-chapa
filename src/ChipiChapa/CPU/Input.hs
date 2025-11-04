{-# LANGUAGE LambdaCase #-}

module ChipiChapa.CPU.Input where

import ChipiChapa.Types
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Raylib.Core

execInput :: (MonadState Chip8 m, MonadIO m) => Input -> m ()
execInput = \case
  SkipIfNotPressed x -> do
    !vx <- use $ registers @ x
    isUp <- liftIO $ isKeyUp $ key vx
    when isUp $ pointer += 2
  SkipIfPressed x -> do
    !vx <- use $ registers @ x
    isDown <- liftIO $ isKeyDown $ key vx
    when isDown $ pointer += 2
  WaitForKey x -> halted .= Waiting x
