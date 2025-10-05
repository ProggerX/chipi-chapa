module ChipiChapa.GUI where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Util.Colors

import ChipiChapa.Types

rLoop :: ChipIO () -> ChipIO ()
rLoop loop = do
  liftIO beginDrawing
  loop
  forM_ [0 .. 63] $ \x -> do
    forM_ [0 .. 31] $ \y -> do
      w <- use $ display @ x
      liftIO . drawRectangle (10 * x) (10 * y) 10 10 $
        if testBit w (32 - y - 1)
          then white
          else black
  liftIO endDrawing
  liftIO windowShouldClose >>= (`unless` rLoop loop)

window :: IO () -> IO ()
window f = do
  win <- initWindow 640 320 "Chipi-chapa"
  setTargetFPS 600
  f
  closeWindow $ Just win
