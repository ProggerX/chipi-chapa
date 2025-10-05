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

import ChipiChapa.Parser
import ChipiChapa.Types

showHex' :: Word16 -> String
showHex' = printf "%04X"

initialChip8 :: Vector Word8 -> Chip8
initialChip8 rom =
  Chip8
    { _pointer = 512
    , _memory = V.concat [V.replicate 512 0, rom, V.replicate (3585 - length rom) 0]
    , _registers = V.replicate 16 0
    , _dt = 0
    , _iReg = 0
    , _stack = []
    , _display = V.replicate 64 0
    , _frame = 0
    }

fromE :: (Show a) => Either a b -> b
fromE (Left x) = error $ show x
fromE (Right a) = a

update :: (MonadState Chip8 m, MonadIO m) => Char -> m ()
update e = do
  -- Timer
  frame %= (\x -> if x > 10 then 0 else x + 1)
  f <- use frame
  when (f == 0) $ dt %= (\x -> if x > 0 then x - 1 else 0)

  -- Events
  case e of
    ' ' -> pure ()
    _ -> pure ()

  -- Execute
  getOpcode
    -- >>= trace
    <&> (fromE . parse parseOpcode "" . showHex')
    -- >>= trace'
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
        if fromIntegral @_ @Int vx + fromIntegral vy > 255
          then (registers @ 15) .= 1
          else (registers @ 15) .= 0
        registers @ x += vy
      Sub x y -> do
        vx <- use $ registers @ x
        vy <- use $ registers @ y
        if vx >= vy
          then registers @ 15 .= 1
          else registers @ 15 .= 0
        registers @ x -= vy
      SubFrom x y -> do
        vx <- use $ registers @ x
        vy <- use $ registers @ y
        if vy >= vx
          then registers @ 15 .= 1
          else registers @ 15 .= 0
        registers @ x .= vy - vx
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
      RegToDelay x -> use dt >>= assign (registers @ x) . fromIntegral
      SetDelay x -> use (registers @ x) >>= assign dt . fromIntegral
      Draw rx ry nv ->
        let
          readRow :: (MonadState Chip8 m) => Int -> m Word8
          readRow i = do
            ci <- use iReg
            use $ memory @ (ci + i)

          drawSprite :: (MonadState Chip8 m, MonadIO m) => Int -> Int -> Int -> m ()
          drawSprite x y n = do
            registers @ 15 .= 0
            forM_ [0 .. min (n - 1) (31 - y)] $ \i -> do
              row <- readRow i
              let cy = i + y

              forM_ [0 .. min 7 (63 - x)] $ \j -> do
                let cx = j + x
                !dx <- use $ display @ cx

                let !px = testBit row (8 - j - 1)

                registers @ 15 %= \vf -> boolToWord8 ((px && testBit dx (32 - cy)) || (vf == 1))
                when px $ display @ cx %= xor (bit (32 - cy))
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
