{-# LANGUAGE LambdaCase #-}

module ChipiChapa.GUI where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Data.Word
import Raylib.Core
import Raylib.Core.Audio
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util.Colors
import Text.Parsec

import ChipiChapa.CPU
import ChipiChapa.Parser
import ChipiChapa.Types

groupPairs :: [a] -> [(a, a)]
groupPairs [] = []
groupPairs [_] = []
groupPairs (x : y : rest) = (x, y) : groupPairs rest

combine :: Word8 -> Word8 -> Word16
combine w1 w2 = (fromIntegral w1 `shiftL` 8) .|. fromIntegral w2

initialGUI :: IO GUI
initialGUI = do
  wv <- loadWaveFromMemory ".wav" $ map fromIntegral $ BS.unpack beepF
  sn <- loadSoundFromWave wv
  pure $ GUI 512 0 sn

rLoop :: AppM ()
rLoop = do
  sa <- use memVAddr
  sn <- use beep
  vc <- use memVCursor
  liftIO (isKeyPressed KeyPageDown) >>= (`when` (memVAddr += 6))
  liftIO (isKeyPressed KeyPageUp) >>= (`when` (memVAddr -= 6))
  liftIO (isKeyPressed KeyRight) >>= (`when` (memVAddr += 1))
  liftIO (isKeyPressed KeyLeft) >>= (`when` (memVAddr -= 1))
  liftIO (isKeyPressed KeyDown) >>= (`when` (if vc >= 8 then memVAddr += 2 else memVCursor += 1))
  liftIO (isKeyPressed KeyUp) >>= (`when` (if vc <= 0 then memVAddr -= 2 else memVCursor -= 1))

  ca <- lift $ use pointer
  liftIO (isKeyPressed KeyN) >>= (`when` (memVAddr .= ca >> memVCursor .= 0))
  spc <- liftIO (isKeyPressed KeySpace)

  lift $ do
    sp <- use speed
    replicateM_ sp update
    dt %= (\t -> if t > 0 then t - 1 else t)

    s <- use st
    liftIO $ do
      when (s > 0) $ playSound sn

    st %= (\t -> if t > 0 then t - 1 else t)

    liftIO beginDrawing

    d <- use debug
    when d $ do
      liftIO $ clearBackground black
      rs <- use registers
      forM_ (zip (V.toList rs) [0 ..]) $ \(b, i) -> do
        liftIO $ drawText "Registers:" 650 20 20 white
        liftIO $
          drawText
            (showHex $ fromIntegral b)
            (650 + (i * 60) `mod` 240)
            (50 + (i `div` 4) * 30)
            20
            white

    forM_ [0 .. 63] $ \x -> do
      forM_ [0 .. 31] $ \y -> do
        w <- use $ display @ x
        liftIO . drawRectangle (10 * x) (10 * y) 10 10 $
          if testBit w (32 - y - 1)
            then white
            else black

      use halted >>= \case
        Working -> pure ()
        Paused -> liftIO $ drawText "Paused (p to resume)" 650 310 20 blue
        AtBreakpoint -> liftIO $ drawText "Stopped at breakpoint" 650 310 20 red
        Waiting _ -> liftIO $ drawText "Waiting for key press" 650 310 20 yellow
      liftIO (drawText ("Speed: " ++ show sp) 650 180 20 white)
      use dt >>= \t -> liftIO (drawText ("DT: " ++ show t) 650 210 20 white)
      use st >>= \t -> liftIO (drawText ("ST: " ++ show t) 730 210 20 white)

      use iReg >>= \i -> liftIO $ do
        drawText "I addr: " 650 250 20 white
        drawText ("0x" ++ showHex (fromIntegral i)) 725 250 20 purple

      use stack >>= \t -> liftIO (drawText ("Stack: " ++ show t) 650 280 20 white)

    liftIO (isKeyPressed KeyEqual) >>= (`when` (speed += 1))
    liftIO (isKeyPressed KeyMinus) >>= (`when` (speed -= 1))

    liftIO (isKeyPressed KeyB) >>= (`when` (breakpoints @ (sa + vc) %= not))
    liftIO (isKeyPressed KeyP)
      >>= ( `when`
              ( halted
                  %= ( \case
                         Paused -> Working
                         AtBreakpoint -> Working
                         _ -> Paused
                     )
              )
          )

    dk <- liftIO $ isKeyPressed KeyF1
    when dk $ do
      debug %= not
      d' <- use debug
      liftIO $ do
        if d'
          then setWindowSize 900 640
          else setWindowSize 640 320

  mem <- lift $ V.toList <$> use memory
  bs <- lift $ use breakpoints
  let l = take 9 $ groupPairs (drop sa mem)
  forM_ (zip l [0 ..]) $ \((v1, v2), i) -> do
    let
      b = combine v1 v2
      hx = showHex $ fromIntegral b
      opc = case parse opcode "" hx of
        Left e -> error (show e)
        Right y -> y
      sh = case opc of
        None -> ""
        x -> show x
    when (spc && vc == i) $ do
      case opc of
        Flow (Goto a) -> do
          memVAddr .= a
          memVCursor .= 0
        Flow (Call a) -> do
          memVAddr .= a
          memVCursor .= 0
        _ -> pure ()
    liftIO $ drawText "Memory:" 20 330 20 white
    liftIO $ do
      drawText
        (show $ i + sa)
        20
        (360 + i * 30)
        20
        gray
      drawText
        hx
        120
        (360 + i * 30)
        20
        (if vc == i then blue else white)
      drawText
        sh
        220
        (360 + i * 30)
        20
        (if vc == i then blue else white)
      when (bs V.! (i + sa)) $ drawRectangle 10 (360 + i * 30) 5 5 red

  liftIO endDrawing
  liftIO windowShouldClose >>= (`unless` rLoop)

window :: IO () -> IO ()
window f = do
  win <- initWindow 640 320 "Chipi chipi chapa chapa dubi dubi daba daba"
  initAudioDevice
  setTargetFPS 60
  f
  closeWindow $ Just win
