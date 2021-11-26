module Lights where

import System.IO
import System.Hardware.Serialport
import Data.Word
import Data.Char (chr)

setDeskColor :: Word8 -> Word8 -> Word8 -> IO ()
setDeskColor r g b = do
  hWithSerial "/dev/ttyUSB0" defaultSerialSettings $ \h -> do
    hPutChar h $ chr $ fromIntegral r
    hPutChar h $ chr $ fromIntegral g
    hPutChar h $ chr $ fromIntegral b

