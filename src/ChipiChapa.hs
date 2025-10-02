module ChipiChapa where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Binary.Get
import Data.ByteString.Lazy qualified as BS
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import System.Environment

import ChipiChapa.CPU
import ChipiChapa.GUI
import ChipiChapa.Types

readRom :: FilePath -> IO (Vector Word8)
readRom fp = do
  contents <- BS.readFile fp
  let cnt = fromIntegral $ BS.length contents

  pure $ runGet (V.replicateM cnt getWord8) contents

getKey :: IO Char
getKey = pure ' '

loop :: ChipIO ()
loop = do
  e <- liftIO getKey
  update e
  drawGUI

drawGUI :: ChipIO ()
drawGUI = pure ()

main :: IO ()
main = window $ do
  arg <- concat <$> getArgs
  rom <- readRom arg
  evalStateT (rLoop loop) $ initialChip8 rom
