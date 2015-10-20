{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Termbox.Internal.Types where

import System.IO.Streams
import System.IO
import Control.Monad.State (MonadState, get, put)
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict (StateT(..))
import Termbox.API.Types
import Data.Monoid
import Data.Typeable
import Control.Exception
import Data.String.Conv
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Foreign.C.Error
import Foreign.C.Types

io :: MonadIO m => IO a -> m a
io = liftIO

data InvariantViolation =
  SyscallFailed !String !CInt !Errno
  deriving (Eq, Typeable)

instance Show InvariantViolation where
  show (SyscallFailed sysName errCode (Errno enoCode)) =
    "Syscall failed: " <> sysName 
                       <> ", exitCode = " 
                       <> show errCode 
                       <> ", errno = " <> show enoCode

instance Exception InvariantViolation

-------------------------------------------------------------------------------
data CellBuf = CellBuf {
    cb_size :: !Rect
  , cb_cells :: ![Cell]
  }

instance Monoid CellBuf where
    mempty = CellBuf mempty mempty
    mappend (CellBuf s1 c1) (CellBuf s2 c2) = CellBuf (s1 <> s2) (c1 <> c2)


-------------------------------------------------------------------------------
data CursorPosition = CursorPosition !Int !Int

pattern CursorHidden  = CursorPosition (-1) (-1)
pattern CursorInvalid = CursorPosition (-2) (-2)

-------------------------------------------------------------------------------
data Rect = Rect {
      rect_w :: !Int
    , rect_h :: !Int
    }

instance Monoid Rect where
    mempty = Rect 0 0
    mappend (Rect w1 h1) (Rect w2 h2) = Rect (w1 + w2) (h1 + h2)

-------------------------------------------------------------------------------
data TermboxState = TermboxState {
    outBuffer         :: !(OutputStream T.Text)
  , keys              :: ![T.Text]
  , funcs             :: ![T.Text]
  , input_mode        :: !InputMode
  , output_mode       :: !OutputMode
  , back_buffer       :: !CellBuf
  , front_buffer      :: !CellBuf
  , foreground        :: !Attribute
  , background        :: !Attribute
  , term_dim          :: !Rect
  , last_fg           :: !Attribute
  , last_bg           :: !Attribute
  , last_cursor_coord :: !CursorPosition
  , cursor            :: !CursorPosition
  , intbuf            :: !Builder
  }

-- 	// termbox inner state
-- 	orig_tios      syscall_Termios
-- 	out            *os.File
-- 	in             int
-- 	inbuf          = make([]byte, 0, 64)
-- 	sigwinch       = make(chan os.Signal, 1)
-- 	sigio          = make(chan os.Signal, 1)
-- 	quit           = make(chan int)
-- 	input_comm     = make(chan input_event)
-- 	interrupt_comm = make(chan struct{})
-- 	intbuf         = make([]byte, 0, 16)

-------------------------------------------------------------------------------
-- The Termbox monad
newtype Termbox a = Termbox { runTermbox :: StateT TermboxState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TermboxState)

-------------------------------------------------------------------------------
intbufAppendInt :: Int -> Termbox ByteString
intbufAppendInt toApp = do
    old@TermboxState{..} <- get
    let newState = intDec toApp <> intbuf
    put $ old { intbuf = newState }
    return . toS $ toLazyByteString newState

-------------------------------------------------------------------------------
writeOut :: T.Text -> Termbox ()
writeOut toWrite = do
    TermboxState{..} <- get
    liftIO $ write (Just toWrite) outBuffer

-------------------------------------------------------------------------------
writeOutBS :: ByteString -> Termbox ()
writeOutBS toWrite = do
    TermboxState{..} <- get
    liftIO $ write (Just . TE.decodeUtf8 $ toWrite) outBuffer

-------------------------------------------------------------------------------
newTermbox :: Handle -> IO TermboxState
newTermbox hdl = do
  oB <- contramap TE.encodeUtf8 =<< handleToOutputStream hdl
  return TermboxState {
    outBuffer  = oB
  , keys  = []
  , funcs = []
  , input_mode  = InputEsc
  , output_mode = OutputNormal
  , back_buffer = mempty
  , front_buffer = mempty
  , foreground = ColorDefault
  , background = ColorDefault
  , term_dim = Rect 0 0
  , last_fg = AttributeInvalid
  , last_bg = AttributeInvalid
  , last_cursor_coord = CursorInvalid
  , cursor = CursorHidden
  , intbuf = mempty
  }

-------------------------------------------------------------------------------
data InputEvent = InputEvent {
    ievt_data :: ByteString
  , ievt_err  :: String
  }

-------------------------------------------------------------------------------
pattern T_enter_ca     = 0
pattern T_exit_ca      = 1
pattern T_show_cursor  = 2
pattern T_hide_cursor  = 3
pattern T_clear_screen = 4
pattern T_sgr0         = 5
pattern T_underline    = 6
pattern T_bold         = 7
pattern T_blink        = 8
pattern T_reverse      = 9
pattern T_enter_keypad = 10
pattern T_exit_keypad  = 11
pattern T_enter_mouse  = 12
pattern T_exit_mouse   = 13
pattern T_max_funcs    = 14

pattern Coord_invalid = -2
pattern Attr_invalid  = Attribute 0xFFFF
