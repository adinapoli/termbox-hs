{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Termbox.Internal.Unsafe.Syscall.IOCtl where

-- http://stackoverflow.com/questions/12806053/get-terminal-width-haskell

import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import Data.Monoid

import           Language.C.Inline.Context
import qualified Language.Haskell.TH as TH
import qualified Language.C.Types as C
import qualified Data.Map.Strict as Map

#include <sys/ioctl.h>

data WinSize = WinSize { 
      wsRow, wsCol, wsXPixel, wsYPixel :: !CUShort
    } deriving (Show, Eq, Ord, Read)

instance Storable WinSize where
  sizeOf _ = (#size struct winsize)
  alignment _ = alignment (undefined :: Ptr WinSize)
  peek ptr = do
    r <- (#peek struct winsize, ws_row) ptr
    c <- (#peek struct winsize, ws_col) ptr
    x <- (#peek struct winsize, ws_xpixel) ptr
    y <- (#peek struct winsize, ws_ypixel) ptr
    return $ WinSize r c x y
  poke ptr WinSize{..} = do
    (#poke struct winsize, ws_row) ptr wsRow
    (#poke struct winsize, ws_col) ptr wsCol
    (#poke struct winsize, ws_xpixel) ptr wsXPixel
    (#poke struct winsize, ws_ypixel) ptr wsYPixel

ioCtlCtx :: Context
ioCtlCtx = baseCtx <> funCtx <> vecCtx <> ctx
  where
    ctx = mempty
      { ctxTypesTable = ioCtlTypesTable
      }

ioCtlTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
ioCtlTypesTable = Map.fromList
  [ 
    (C.TypeName "winsize", [t| WinSize |])
  ]
