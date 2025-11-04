{-# LANGUAGE LambdaCase #-}
module ChipiChapa.CPU.Math where

import ChipiChapa.Types
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Bits

execMath :: (MonadState Chip8 m, MonadIO m) => Math -> m ()
execMath = \case
          CAdd x nn -> (registers @ x) += nn
          BOr x y ->
            use (registers @ y)
              >>= (\vx vy -> vx %= (.|.) vy) (registers @ x)
          BAnd x y ->
            use (registers @ y)
              >>= (\vx vy -> vx %= (.&.) vy) (registers @ x)
          BXor x y ->
            use (registers @ y)
              >>= (\vx vy -> vx %= xor vy) (registers @ x)
          Add x y -> do
            !vx <- use $ registers @ x
            !vy <- use $ registers @ y
            registers @ x += vy
            if fromIntegral @_ @Int vx + fromIntegral vy > 255
              then (registers @ 15) .= 1
              else (registers @ 15) .= 0
          Sub x y -> do
            !vx <- use $ registers @ x
            !vy <- use $ registers @ y
            registers @ x -= vy
            if vx >= vy
              then registers @ 15 .= 1
              else registers @ 15 .= 0
          SubFrom x y -> do
            !vx <- use $ registers @ x
            !vy <- use $ registers @ y
            registers @ x .= vy - vx
            if vy >= vx
              then registers @ 15 .= 1
              else registers @ 15 .= 0
          RShift x -> do
            registers @ x %= (`shiftR` 1)
            vx <- use (registers @ x)
            registers @ 15 .= fromIntegral (vx .&. 1)
          LShift x -> do
            registers @ x %= (`shiftL` 1)
            vx <- use (registers @ x)
            registers @ 15 .= fromIntegral (vx .&. bit 8)
