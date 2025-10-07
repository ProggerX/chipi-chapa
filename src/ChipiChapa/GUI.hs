{-# LANGUAGE LambdaCase #-}

module ChipiChapa.GUI where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.Vector qualified as V
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util.Colors

import ChipiChapa.CPU
import ChipiChapa.Types

rLoop :: ChipIO ()
rLoop = do
  sp <- use speed
  replicateM_ sp update
  dt %= (\t -> if t > 0 then t - 1 else t)

  liftIO beginDrawing

  forM_ [0 .. 63] $ \x -> do
    forM_ [0 .. 31] $ \y -> do
      w <- use $ display @ x
      liftIO . drawRectangle (10 * x) (10 * y) 10 10 $
        if testBit w (32 - y - 1)
          then white
          else black

  d <- use debug
  when d $ do
    liftIO $ drawRectangle 640 0 960 640 black
    rs <- use registers
    forM_ (zip (V.toList rs) [0 ..]) $ \(b, i) -> do
      liftIO $ drawText "Registers:" 650 0 20 white
      liftIO $
        drawText
          (showHex' $ fromIntegral b)
          (650 + (i * 60) `mod` 240)
          (30 + (i `div` 4) * 30)
          20
          white

    use halted >>= \case
      -1 -> pure ()
      17 -> liftIO $ drawText "Paused (p to resume)" 650 150 20 blue
      _ -> liftIO $ drawText "Waiting for key press" 650 150 20 yellow
    liftIO (drawText ("Speed: " ++ show sp) 650 180 20 white)
    use dt >>= \t -> liftIO (drawText ("DT: " ++ show t) 650 210 20 white)

    use iReg >>= \i -> liftIO $ do
      drawText "I addr: " 650 250 20 white
      drawText ("0x" ++ showHex' (fromIntegral i)) 725 250 20 purple

    use stack >>= \st -> liftIO (drawText ("Stack: " ++ show st) 650 280 20 white)

  liftIO (isKeyPressed KeyEqual) >>= (`when` (speed += 1))
  liftIO (isKeyPressed KeyMinus) >>= (`when` (speed -= 1))

  liftIO (isKeyPressed KeyP)
    >>= ( `when`
            (halted %= (\x -> if x == -1 then 17 else -1))
        )

  dk <- liftIO $ isKeyPressed KeyF1
  when dk $ do
    debug %= not
    d' <- use debug
    liftIO $ do
      if d'
        then setWindowSize 900 320
        else setWindowSize 640 320

  liftIO endDrawing
  liftIO windowShouldClose >>= (`unless` rLoop)

window :: IO () -> IO ()
window f = do
  win <- initWindow 640 320 "Chipi-chapa"
  setTargetFPS 60
  f
  closeWindow $ Just win
