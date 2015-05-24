{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Termbox where

import Termbox.Internal.Types
import Termbox.API.Types
import Foreign.Storable
import Control.Monad.State (gets)

import           Language.C.Inline
import qualified Language.C.Inline as C
import Foreign.ForeignPtr
import           Termbox.Internal.Unsafe.Syscall.IOCtl

context ioCtlCtx
C.include "<sys/ioctl.h>"

unsafeGetWinSize :: IO (Either CInt WinSize)
unsafeGetWinSize = do
  wsPtrF <- mallocForeignPtr
  withForeignPtr wsPtrF $ \wsPtr -> do
    eC <- [C.exp| int { ioctl(0, TIOCGWINSZ, $(winsize * wsPtr)) } |]
    case eC of
        0 -> Right <$> peek wsPtr
        _ -> return $ Left eC

--------------------------------------------------------------------------------
writeCursor :: CursorPosition -> Termbox ()
writeCursor (CursorPosition x y) = do
  writeOut "\033["
  intbufAppendInt (y + 1) >>= writeOutBS 
  writeOut ";"
  intbufAppendInt (x + 1) >>= writeOutBS 
  writeOut "H"

--------------------------------------------------------------------------------
onSGR :: Termbox ()
      -> Termbox ()
      -> Termbox ()
onSGR onMatch defCase = do
  oMode <- gets output_mode
  go oMode
  where
    go oMode
      | oMode == Output256 || oMode == Output216 || oMode == OutputGrayscale =
        onMatch
      | otherwise = defCase >> writeOut "m"

--------------------------------------------------------------------------------
writeSgrFg :: Attribute -> Termbox ()
writeSgrFg (Attribute a) = onSGR onOK onKO
  where
    onOK = do
      writeOut "\033[38;5;"
      intbufAppendInt (fromIntegral a - 1) >>= writeOutBS 
    onKO = do
      writeOut "\033[3"
      intbufAppendInt (fromIntegral a - 1) >>= writeOutBS 

--------------------------------------------------------------------------------
writeSgrBg :: Attribute -> Termbox ()
writeSgrBg (Attribute a) = onSGR onOK onKO
  where
    onOK = do
      writeOut "\033[48;5;"
      intbufAppendInt (fromIntegral a - 1) >>= writeOutBS 
    onKO = do
      writeOut "\033[4"
      intbufAppendInt (fromIntegral a - 1) >>= writeOutBS 

--------------------------------------------------------------------------------
-- TODO: This function can be refactored further.
writeSgr :: Attribute -> Attribute -> Termbox ()
writeSgr (Attribute fg) (Attribute bg) = onSGR onOK onKO
  where
    onOK = do
      writeOut "\033[38;5;"
      intbufAppendInt (fromIntegral fg - 1) >>= writeOutBS 
      writeOut "m"
      writeOut "\033[48;5;"
      intbufAppendInt (fromIntegral bg - 1) >>= writeOutBS 
    onKO = do
      writeOut "\033[3"
      intbufAppendInt (fromIntegral fg - 1) >>= writeOutBS 
      writeOut ";4"
      intbufAppendInt (fromIntegral bg - 1) >>= writeOutBS 


{-
type winsize struct {
	rows    uint16
	cols    uint16
	xpixels uint16
	ypixels uint16
}

getWinSize :: Termbox WinSize
getWinSize = do
  win

func get_term_size(fd uintptr) (int, int) {
	var sz winsize
	_, _, _ = syscall.Syscall(syscall.SYS_IOCTL,
		fd, uintptr(syscall.TIOCGWINSZ), uintptr(unsafe.Pointer(&sz)))
	return int(sz.cols), int(sz.rows)
}

func send_attr(fg, bg Attribute) {
	if fg == lastfg && bg == lastbg {
		return
	}

	outbuf.WriteString(funcs[t_sgr0])

	var fgcol, bgcol Attribute

	switch output_mode {
	case Output256:
		fgcol = fg & 0x1FF
		bgcol = bg & 0x1FF
	case Output216:
		fgcol = fg & 0xFF
		bgcol = bg & 0xFF
		if fgcol > 216 {
			fgcol = ColorDefault
		}
		if bgcol > 216 {
			bgcol = ColorDefault
		}
		if fgcol != ColorDefault {
			fgcol += 0x10
		}
		if bgcol != ColorDefault {
			bgcol += 0x10
		}
	case OutputGrayscale:
		fgcol = fg & 0x1F
		bgcol = bg & 0x1F
		if fgcol > 24 {
			fgcol = ColorDefault
		}
		if bgcol > 24 {
			bgcol = ColorDefault
		}
		if fgcol != ColorDefault {
			fgcol += 0xe8
		}
		if bgcol != ColorDefault {
			bgcol += 0xe8
		}
	default:
		fgcol = fg & 0x0F
		bgcol = bg & 0x0F
	}

	if fgcol != ColorDefault {
		if bgcol != ColorDefault {
			write_sgr(fgcol, bgcol)
		} else {
			write_sgr_fg(fgcol)
		}
	} else if bgcol != ColorDefault {
		write_sgr_bg(bgcol)
	}

	if fg&AttrBold != 0 {
		outbuf.WriteString(funcs[t_bold])
	}
	if bg&AttrBold != 0 {
		outbuf.WriteString(funcs[t_blink])
	}
	if fg&AttrUnderline != 0 {
		outbuf.WriteString(funcs[t_underline])
	}
	if fg&AttrReverse|bg&AttrReverse != 0 {
		outbuf.WriteString(funcs[t_reverse])
	}

	lastfg, lastbg = fg, bg
}

func send_char(x, y int, ch rune) {
	var buf [8]byte
	n := utf8.EncodeRune(buf[:], ch)
	if x-1 != lastx || y != lasty {
		write_cursor(x, y)
	}
	lastx, lasty = x, y
	outbuf.Write(buf[:n])
}

func flush() error {
	_, err := io.Copy(out, &outbuf)
	outbuf.Reset()
	if err != nil {
		return err
	}
	return nil
}

func send_clear() error {
	send_attr(foreground, background)
	outbuf.WriteString(funcs[t_clear_screen])
	if !is_cursor_hidden(cursor_x, cursor_y) {
		write_cursor(cursor_x, cursor_y)
	}

	// we need to invalidate cursor position too and these two vars are
	// used only for simple cursor positioning optimization, cursor
	// actually may be in the correct place, but we simply discard
	// optimization once and it gives us simple solution for the case when
	// cursor moved
	lastx = coord_invalid
	lasty = coord_invalid

	return flush()
}

func update_size_maybe() error {
	w, h := get_term_size(out.Fd())
	if w != termw || h != termh {
		termw, termh = w, h
		back_buffer.resize(termw, termh)
		front_buffer.resize(termw, termh)
		front_buffer.clear()
		return send_clear()
	}
	return nil
}

func tcsetattr(fd uintptr, termios *syscall_Termios) error {
	r, _, e := syscall.Syscall(syscall.SYS_IOCTL,
		fd, uintptr(syscall_TCSETS), uintptr(unsafe.Pointer(termios)))
	if r != 0 {
		return os.NewSyscallError("SYS_IOCTL", e)
	}
	return nil
}

func tcgetattr(fd uintptr, termios *syscall_Termios) error {
	r, _, e := syscall.Syscall(syscall.SYS_IOCTL,
		fd, uintptr(syscall_TCGETS), uintptr(unsafe.Pointer(termios)))
	if r != 0 {
		return os.NewSyscallError("SYS_IOCTL", e)
	}
	return nil
}

func parse_escape_sequence(event *Event, buf []byte) (int, bool) {
	bufstr := string(buf)
	// mouse
	if len(bufstr) >= 6 && strings.HasPrefix(bufstr, "\033[M") {
		switch buf[3] & 3 {
		case 0:
			event.Key = MouseLeft
		case 1:
			event.Key = MouseMiddle
		case 2:
			event.Key = MouseRight
		case 3:
			return 6, false
		}
		event.Type = EventMouse // KeyEvent by default
		// wheel up outputs MouseLeft
		if buf[3] == 0x60 || buf[3] == 0x70 {
			event.Key = MouseMiddle
		}
		// the coord is 1,1 for upper left
		event.MouseX = int(buf[4]) - 1 - 32
		event.MouseY = int(buf[5]) - 1 - 32
		return 6, true
	}

	for i, key := range keys {
		if strings.HasPrefix(bufstr, key) {
			event.Ch = 0
			event.Key = Key(0xFFFF - i)
			return len(key), true
		}
	}
	return 0, true
}

func extract_raw_event(data []byte, event *Event) bool {
	if len(inbuf) == 0 {
		return false
	}

	n := len(data)
	if n == 0 {
		return false
	}

	n = copy(data, inbuf)
	copy(inbuf, inbuf[n:])
	inbuf = inbuf[:len(inbuf)-n]

	event.N = n
	event.Type = EventRaw
	return true
}

func extract_event(inbuf []byte, event *Event) bool {
	if len(inbuf) == 0 {
		event.N = 0
		return false
	}

	if inbuf[0] == '\033' {
		// possible escape sequence
		n, ok := parse_escape_sequence(event, inbuf)
		if n != 0 {
			event.N = n
			return ok
		}

		// it's not escape sequence, then it's Alt or Esc, check input_mode
		switch {
		case input_mode&InputEsc != 0:
			// if we're in escape mode, fill Esc event, pop buffer, return success
			event.Ch = 0
			event.Key = KeyEsc
			event.Mod = 0
			event.N = 1
			return true
		case input_mode&InputAlt != 0:
			// if we're in alt mode, set Alt modifier to event and redo parsing
			event.Mod = ModAlt
			ok := extract_event(inbuf[1:], event)
			if ok {
				event.N++
			} else {
				event.N = 0
			}
			return ok
		default:
			panic("unreachable")
		}
	}

	// if we're here, this is not an escape sequence and not an alt sequence
	// so, it's a FUNCTIONAL KEY or a UNICODE character

	// first of all check if it's a functional key
	if Key(inbuf[0]) <= KeySpace || Key(inbuf[0]) == KeyBackspace2 {
		// fill event, pop buffer, return success
		event.Ch = 0
		event.Key = Key(inbuf[0])
		event.N = 1
		return true
	}

	// the only possible option is utf8 rune
	if r, n := utf8.DecodeRune(inbuf); r != utf8.RuneError {
		event.Ch = r
		event.Key = 0
		event.N = n
		return true
	}

	return false
}

func fcntl(fd int, cmd int, arg int) (val int, err error) {
	r, _, e := syscall.Syscall(syscall.SYS_FCNTL, uintptr(fd), uintptr(cmd),
		uintptr(arg))
	val = int(r)
	if e != 0 {
		err = e
	}
	return
}
-}
