{-# LANGUAGE LambdaCase #-}
module ChipiChapa.CPU.Input where

import ChipiChapa.Types
import Control.Lens
import Control.Monad.IO.Class
import Raylib.Core
import Control.Monad.State
import Control.Monad

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
