{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

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

import ChipiChapa.Parser
import ChipiChapa.Types

import Raylib.Core

showHex' :: Word16 -> String
showHex' = printf "%04X"

initialChip8 :: Vector Word8 -> Chip8
initialChip8 rom =
  Chip8
    { _pointer = 512
    , _memory =
        V.concat
          [ fontData
          , V.replicate (512 - length fontData) 0
          , rom
          , V.replicate (3585 - length rom) 0
          ]
    , _registers = V.replicate 16 0
    , _dt = 0
    , _iReg = 0
    , _stack = []
    , _display = V.replicate 64 0
    , _halted = -1
    , _debug = False
    , _speed = 10
    }

fromE :: (Show a) => Either a b -> b
fromE (Left x) = error $ show x
fromE (Right a) = a

update :: (MonadState Chip8 m, MonadIO m) => m ()
update = do
  h <- use halted

  if
    | h > -1 && h < 16 -> do
        forM_ keys $ \k -> do
          d <- liftIO $ isKeyDown k
          when d $ do
            registers @ h .= key' k
            halted .= -1
    | h > -1 -> pure ()
    | otherwise ->
        getOpcode
          >>= trace
          <&> (fromE . parse parseOpcode "" . showHex')
          >>= trace'
          >>= (\a -> pointer += 2 >> pure a)
          >>= \case
            None -> pure ()
            DispClear -> display .= V.replicate 64 0
            Goto nnn -> pointer .= nnn
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
            RegSet x nn -> (registers @ x) .= nn
            CAdd x nn -> (registers @ x) += nn
            Move x y -> use (registers @ y) >>= assign (registers @ x)
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
            SetI nnn -> iReg .= nnn
            JmpV0Plus nnn -> do
              v0 <- fromIntegral <$> use (registers @ 0)
              pointer .= v0 + nnn
            RandomAnd x nn -> do
              rnd <- liftIO $ randomRIO (0, 255)
              registers @ x .= rnd .&. nn
            GetDelay x -> use dt >>= assign (registers @ x) . fromIntegral
            SetDelay x -> use (registers @ x) >>= assign dt . fromIntegral
            SkipIfNotPressed x -> do
              !vx <- use $ registers @ x
              isUp <- liftIO $ isKeyUp $ key vx
              when isUp $ pointer += 2
            SkipIfPressed x -> do
              !vx <- use $ registers @ x
              isDown <- liftIO $ isKeyDown $ key vx
              when isDown $ pointer += 2
            WaitForKey x -> halted .= x
            AddI x -> use (registers @ x) >>= (iReg +=) . fromIntegral
            StoreBCD x -> do
              vx <- use $ registers @ x

              i <- use iReg
              memory @ (i + 2) .= vx `mod` 10
              memory @ (i + 1) .= vx `div` 10
              memory @ i .= vx `div` 100
            DumpRegs x -> do
              i <- use iReg
              forM_ [0 .. x] $ \l -> do
                r <- use $ registers @ l
                memory @ (i + l) .= r
            LoadRegs x -> do
              i <- use iReg
              forM_ [0 .. x] $ \l -> do
                r <- use $ memory @ (i + l)
                registers @ l .= r
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
