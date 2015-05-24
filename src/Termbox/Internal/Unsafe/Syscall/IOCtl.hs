module Termbox.Internal.Unsafe.Syscall.IOCtl where

-- http://stackoverflow.com/questions/12806053/get-terminal-width-haskell

import Foreign.Storable
import Foreign.Ptr
import Foreign.C

import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types as C

data WinSize = WinSize { 
      wsRows, wsCols, wsXPixels, wsYPixels :: CUShort
    }

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr WinSize -> IO CInt
