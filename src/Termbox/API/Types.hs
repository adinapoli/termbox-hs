{-# LANGUAGE PatternSynonyms #-}
module Termbox.API.Types where

import Data.Word

newtype InputMode  = InputMode Int
newtype OutputMode = OutputMode Int
newtype EventType  = EventType Word8
newtype Modifier   = Modifier Word8
newtype Key        = Key Word16
newtype Attribute  = Attribute Word16

-------------------------------------------------------------------------------
-- This type represents a termbox event. The 'Mod', 'Key' and 'Ch' fields are
-- valid if 'Type' is EventKey. The 'Width' and 'Height' fields are valid if
-- 'Type' is EventResize. The 'Err' field is valid if 'Type' is EventError.
data Event = Event {
    evt_type   :: EventType
    -- ^ one of Event* constants
  , evt_mod    :: Modifier
    -- ^ one of Mod* constants or 0
  , evt_key    :: Key
    -- ^ one of Key* constants, invalid if 'Ch' is not 0
  , evt_ch     :: Word32
    -- ^ a unicode character
  , evt_width  :: Int
    -- ^ width of the screen
  , evt_height :: Int
    -- ^ height of the screen
  , evt_err    :: String
    -- ^ error in case if input failed
  , evt_mouseX :: Int
    -- ^ x coord of mouse
  , evt_mouseY :: Int
    -- ^ y coord of mouse
  , evt_n      :: Int
    -- ^ number of bytes written when getting a raw event
  }

-------------------------------------------------------------------------------
-- A cell, single conceptual entity on the screen. The screen is basically a 2d
-- array of cells. 'Ch' is a unicode character, 'Fg' and 'Bg' are foreground
-- and background attributes respectively.
data Cell = Cell {
    cell_ch :: Word32
  , cell_fg :: Attribute
  , cell_bg :: Attribute
}

-------------------------------------------------------------------------------
-- Key constants, see Event.Key field.
pattern KeyF1         = Key 4095
pattern KeyF2         = Key 4094
pattern KeyF3         = Key 4093
pattern KeyF4         = Key 4092
pattern KeyF5         = Key 4091
pattern KeyF6         = Key 4090
pattern KeyF7         = Key 4089
pattern KeyF8         = Key 4088
pattern KeyF9         = Key 4087
pattern KeyF10        = Key 4086
pattern KeyF11        = Key 4085
pattern KeyF12        = Key 4084
pattern KeyInsert     = Key 4083
pattern KeyDelete     = Key 4082
pattern KeyHome       = Key 4081
pattern KeyEnd        = Key 4080
pattern KeyPgup       = Key 4079
pattern KeyPgdn       = Key 4078
pattern KeyArrowUp    = Key 4077
pattern KeyArrowDown  = Key 4076
pattern KeyArrowLeft  = Key 4075
pattern KeyArrowRight = Key 4074
pattern Key_min       = Key 4073
pattern MouseLeft     = Key 4072
pattern MouseMiddle   = Key 4071
pattern MouseRight    = Key 4070

pattern KeyCtrlTilde      = Key 0x00
pattern KeyCtrl2          = Key 0x00
pattern KeyCtrlSpace      = Key 0x00
pattern KeyCtrlA          = Key 0x01
pattern KeyCtrlB          = Key 0x02
pattern KeyCtrlC          = Key 0x03
pattern KeyCtrlD          = Key 0x04
pattern KeyCtrlE          = Key 0x05
pattern KeyCtrlF          = Key 0x06
pattern KeyCtrlG          = Key 0x07
pattern KeyBackspace      = Key 0x08
pattern KeyCtrlH          = Key 0x08
pattern KeyTab            = Key 0x09
pattern KeyCtrlI          = Key 0x09
pattern KeyCtrlJ          = Key 0x0A
pattern KeyCtrlK          = Key 0x0B
pattern KeyCtrlL          = Key 0x0C
pattern KeyEnter          = Key 0x0D
pattern KeyCtrlM          = Key 0x0D
pattern KeyCtrlN          = Key 0x0E
pattern KeyCtrlO          = Key 0x0F
pattern KeyCtrlP          = Key 0x10
pattern KeyCtrlQ          = Key 0x11
pattern KeyCtrlR          = Key 0x12
pattern KeyCtrlS          = Key 0x13
pattern KeyCtrlT          = Key 0x14
pattern KeyCtrlU          = Key 0x15
pattern KeyCtrlV          = Key 0x16
pattern KeyCtrlW          = Key 0x17
pattern KeyCtrlX          = Key 0x18
pattern KeyCtrlY          = Key 0x19
pattern KeyCtrlZ          = Key 0x1A
pattern KeyEsc            = Key 0x1B
pattern KeyCtrlLsqBracket = Key 0x1B
pattern KeyCtrl3          = Key 0x1B
pattern KeyCtrl4          = Key 0x1C
pattern KeyCtrlBackslash  = Key 0x1C
pattern KeyCtrl5          = Key 0x1D
pattern KeyCtrlRsqBracket = Key 0x1D
pattern KeyCtrl6          = Key 0x1E
pattern KeyCtrl7          = Key 0x1F
pattern KeyCtrlSlash      = Key 0x1F
pattern KeyCtrlUnderscore = Key 0x1F
pattern KeySpace          = Key 0x20
pattern KeyBackspace2     = Key 0x7F
pattern KeyCtrl8          = Key 0x7F

-------------------------------------------------------------------------------
-- Alt modifier constant, see Event.Mod field and SetInputMode function.
pattern ModAlt = Modifier 0x01

-------------------------------------------------------------------------------
-- Cell colors, you can combine a color with multiple attributes using bitwise
-- OR ('|').
pattern ColorDefault = Attribute 0x00
pattern ColorBlack   = Attribute 0x01
pattern ColorRed     = Attribute 0x02
pattern ColorGreen   = Attribute 0x03
pattern ColorYellow  = Attribute 0x4
pattern ColorBlue    = Attribute 0x5
pattern ColorMagenta = Attribute 0x6
pattern ColorCyan    = Attribute 0x7
pattern ColorWhite   = Attribute 0x8

-------------------------------------------------------------------------------
-- Cell attributes, it is possible to use multiple attributes by combining them
-- using bitwise OR ('|'). Although, colors cannot be combined. But you can
-- combine attributes and a single color.
--
-- It's worth mentioning that some platforms don't support certain attibutes.
-- For example windows console doesn't support AttrUnderline. And on some
-- terminals applying AttrBold to background may result in blinking text. Use
-- them with caution and test your code on various terminals.
pattern AttrBold      = Attribute 0x0100
pattern AttrUnderline = Attribute 0x0200
pattern AttrReverse   = Attribute 0x0400

-------------------------------------------------------------------------------
-- Input mode. See SetInputMode function.
pattern InputCurrent = InputMode 0
pattern InputEsc = InputMode 1
pattern InputAlt = InputMode 2
pattern InputMouse = InputMode 4

-------------------------------------------------------------------------------
-- Output mode. See SetOutputMode function.
pattern OutputCurrent   = OutputMode 1
pattern OutputNormal    = OutputMode 2
pattern Output256       = OutputMode 3
pattern Output216       = OutputMode 4
pattern OutputGrayscale = OutputMode 5

-------------------------------------------------------------------------------
-- Event type. See Event.Type field.
pattern EventKey       = EventType 0x00
pattern EventResize    = EventType 0x01
pattern EventMouse     = EventType 0x02
pattern EventError     = EventType 0x03
pattern EventInterrupt = EventType 0x04
pattern EventRaw       = EventType 0x05
pattern EventNone      = EventType 0x06

