module ChipiChapa.GUI where

import Control.Monad
import Control.Monad.IO.Class
import Raylib.Core

import ChipiChapa.Types

rLoop :: ChipIO () -> ChipIO ()
rLoop loop = do
  liftIO beginDrawing
  loop
  liftIO endDrawing
  liftIO windowShouldClose >>= (`unless` rLoop loop)

window :: IO () -> IO ()
window f = do
  win <- initWindow 800 800 "Chipi-chapa"
  setTargetFPS 60
  f
  closeWindow $ Just win
