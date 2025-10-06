module ChipiChapa.GUI where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Util.Colors

import ChipiChapa.CPU
import ChipiChapa.Types

rLoop :: ChipIO ()
rLoop = do
  replicateM_ 10 update
  dt %= (\t -> if t > 0 then t - 1 else t)
  liftIO beginDrawing
  forM_ [0 .. 63] $ \x -> do
    forM_ [0 .. 31] $ \y -> do
      w <- use $ display @ x
      liftIO . drawRectangle (10 * x) (10 * y) 10 10 $
        if testBit w (32 - y - 1)
          then white
          else black
  liftIO endDrawing
  liftIO windowShouldClose >>= (`unless` rLoop)

window :: IO () -> IO ()
window f = do
  win <- initWindow 640 320 "Chipi-chapa"
  setTargetFPS 60
  f
  closeWindow $ Just win
