module ChipiChapa where

import Control.Monad.State
import Data.Binary.Get
import Data.ByteString.Lazy qualified as BS
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word
import System.Environment

import ChipiChapa.CPU
import ChipiChapa.GUI

readRom :: FilePath -> IO (Vector Word8)
readRom fp = do
  contents <- BS.readFile fp
  let cnt = fromIntegral $ BS.length contents

  pure $ runGet (V.replicateM cnt getWord8) contents

main :: IO ()
main = window $ do
  arg <- concat <$> getArgs
  rom <- readRom arg
  evalStateT
    (evalStateT rLoop initialGUI)
    $ initialChip8 rom
