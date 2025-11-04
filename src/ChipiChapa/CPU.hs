{-# LANGUAGE LambdaCase #-}

module ChipiChapa.CPU where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Bits
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import System.Random
import Text.Parsec
import Text.Printf

import ChipiChapa.CPU.Flow
import ChipiChapa.CPU.Input
import ChipiChapa.CPU.Math
import ChipiChapa.CPU.Regs
import ChipiChapa.Parser
import ChipiChapa.Types

import Raylib.Core

showHex' :: Word16 -> String
showHex' = printf "%04X"

initialChip8 :: Vector Word8 -> Chip8
initialChip8 rom =
  Chip8
    { _pointer = 512
    , _breakpoints = V.replicate 4096 False
    , _memory =
        V.concat
          [ fontData
          , V.replicate (512 - length fontData) 0
          , rom
          , V.replicate (3585 - length rom) 0
          ]
    , _registers = V.replicate 16 0
    , _stack = []
    , _dt = 0
    , _st = 0
    , _iReg = 0
    , _display = V.replicate 64 0
    , _halted = Working
    , _debug = False
    , _speed = 10
    }

fromE :: (Show a) => Either a b -> b
fromE (Left x) = error $ show x
fromE (Right a) = a

update :: (MonadState Chip8 m, MonadIO m) => m ()
update = do
  h <- use halted
  bs <- use breakpoints
  pc <- use pointer

  case h of
    Waiting r -> do
      forM_ keys $ \k -> do
        d <- liftIO $ isKeyDown k
        when d $ do
          registers @ r .= key' k
          halted .= Working
    Paused -> pure ()
    AtBreakpoint -> pure ()
    Working -> do
      when (bs V.! pc) $ halted .= AtBreakpoint
      getOpcode
        >>= trace
        <&> (fromE . parse opcode "" . showHex')
        >>= trace'
        >>= (\a -> pointer += 2 >> pure a)
        >>= \case
          None -> pure ()
          Flow f -> execFlow f
          Regs r -> execRegs r
          Math m -> execMath m
          Input i -> execInput i
          DispClear -> display .= V.replicate 64 0
          RandomAnd x nn -> do
            rnd <- liftIO $ randomRIO (0, 255)
            registers @ x .= rnd .&. nn
          GetDelay x -> use dt >>= assign (registers @ x) . fromIntegral
          SetDelay x -> use (registers @ x) >>= assign dt . fromIntegral
          SetSound x -> use (registers @ x) >>= assign st . fromIntegral
          FontSprite x -> do
            vx <- use $ registers @ x
            iReg .= fromIntegral vx * 5
          Draw rx ry nv ->
            let
              readRow :: (MonadState Chip8 m) => Int -> m Word8
              readRow i = do
                ci <- use iReg
                use $ memory @ (ci + i)

              drawSprite :: (MonadState Chip8 m, MonadIO m) => Int -> Int -> Int -> m ()
              drawSprite x y n = do
                registers @ 15 .= 0
                forM_ [0 .. n - 1] $ \i -> do
                  row <- readRow i
                  let cy = (i + y) `mod` 32

                  forM_ [0 .. 7] $ \j -> do
                    let cx = (j + x) `mod` 64
                    !dx <- use $ display @ cx

                    let !px = testBit row (8 - j - 1)

                    registers @ 15 %= \vf -> boolToWord8 ((px && testBit dx (32 - cy - 1)) || (vf == 1))
                    when px $ display @ cx %= xor (bit (32 - cy - 1))
             in
              do
                vx <- fromIntegral <$> use (registers @ rx)
                vy <- fromIntegral <$> use (registers @ ry)
                drawSprite vx vy nv
 where
  combine :: Word8 -> Word8 -> Word16
  combine w1 w2 = (fromIntegral w1 `shiftL` 8) .|. fromIntegral w2

  boolToWord8 :: Bool -> Word8
  boolToWord8 True = 1
  boolToWord8 False = 0

  trace :: (MonadIO m) => Word16 -> m Word16
  trace x = liftIO (putStrLn $ showHex' x) >> pure x

  trace' :: (MonadIO m) => Opcode -> m Opcode
  trace' x = liftIO (print x) >> pure x

  getOpcode :: (MonadState Chip8 m) => m Word16
  getOpcode = do
    ptr <- use pointer
    b1 <- use $ memory @ ptr
    b2 <- use $ memory @ (ptr + 1)
    pure $ combine b1 b2
