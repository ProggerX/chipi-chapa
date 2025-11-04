{-# LANGUAGE LambdaCase #-}

module ChipiChapa.Types.Keyboard where

import Data.Word
import Raylib.Types

key :: Word8 -> KeyboardKey
key = \case
  0x1 -> KeyOne
  0x2 -> KeyTwo
  0x3 -> KeyThree
  0xC -> KeyFour
  0x4 -> KeyQ
  0x5 -> KeyW
  0x6 -> KeyE
  0xD -> KeyR
  0x7 -> KeyA
  0x8 -> KeyS
  0x9 -> KeyD
  0xE -> KeyF
  0xA -> KeyZ
  0x0 -> KeyX
  0xB -> KeyC
  0xF -> KeyV
  _ -> undefined

key' :: KeyboardKey -> Word8
key' = \case
  KeyOne -> 0x1
  KeyTwo -> 0x2
  KeyThree -> 0x3
  KeyFour -> 0xC
  KeyQ -> 0x4
  KeyW -> 0x5
  KeyE -> 0x6
  KeyR -> 0xD
  KeyA -> 0x7
  KeyS -> 0x8
  KeyD -> 0x9
  KeyF -> 0xE
  KeyZ -> 0xA
  KeyX -> 0x0
  KeyC -> 0xB
  KeyV -> 0xF
  _ -> 0x0

keys :: [KeyboardKey]
keys =
  [ KeyOne
  , KeyTwo
  , KeyThree
  , KeyFour
  , KeyQ
  , KeyW
  , KeyE
  , KeyR
  , KeyA
  , KeyS
  , KeyD
  , KeyF
  , KeyZ
  , KeyX
  , KeyC
  , KeyV
  ]
