module Ivored.Keycodes where


key_MOD_LCTRL  = 0x01
key_MOD_LSHIFT = 0x02
key_MOD_LALT   = 0x04
key_MOD_LMETA  = 0x08
key_MOD_RCTRL  = 0x10
key_MOD_RSHIFT = 0x20
key_MOD_RALT   = 0x40
key_MOD_RMETA  = 0x80

-- /**
--  * Scan codes - last N slots in the HID report (usually 6).
--  * 0x00 if no key pressed.
--  *
--  * If more than N keys are pressed, the HID reports
--  * KEY_ERR_OVF in all slots to indicate this condition.
--  */

key_NONE    = 0x00  -- No key pressed
key_ERR_OVF = 0x01  --  Keyboard Error Roll Over - used for all slots if too many keys are pressed ("Phantom key")
-- 0x02  --  Keyboard POST Fail
-- 0x03  --  Keyboard Error Undefined
key_A = 0x04  -- Keyboard a and A
key_B = 0x05  -- Keyboard b and B
key_C = 0x06  -- Keyboard c and C
key_D = 0x07  -- Keyboard d and D
key_E = 0x08  -- Keyboard e and E
key_F = 0x09  -- Keyboard f and F
key_G = 0x0a  -- Keyboard g and G
key_H = 0x0b  -- Keyboard h and H
key_I = 0x0c  -- Keyboard i and I
key_J = 0x0d  -- Keyboard j and J
key_K = 0x0e  -- Keyboard k and K
key_L = 0x0f  -- Keyboard l and L
key_M = 0x10  -- Keyboard m and M
key_N = 0x11  -- Keyboard n and N
key_O = 0x12  -- Keyboard o and O
key_P = 0x13  -- Keyboard p and P
key_Q = 0x14  -- Keyboard q and Q
key_R = 0x15  -- Keyboard r and R
key_S = 0x16  -- Keyboard s and S
key_T = 0x17  -- Keyboard t and T
key_U = 0x18  -- Keyboard u and U
key_V = 0x19  -- Keyboard v and V
key_W = 0x1a  -- Keyboard w and W
key_X = 0x1b  -- Keyboard x and X
key_Y = 0x1c  -- Keyboard y and Y
key_Z = 0x1d  -- Keyboard z and Z

key_1 = 0x1e  -- Keyboard 1 and !
key_2 = 0x1f  -- Keyboard 2 and @
key_3 = 0x20  -- Keyboard 3 and #
key_4 = 0x21  -- Keyboard 4 and $
key_5 = 0x22  -- Keyboard 5 and %
key_6 = 0x23  -- Keyboard 6 and ^
key_7 = 0x24  -- Keyboard 7 and &
key_8 = 0x25  -- Keyboard 8 and *
key_9 = 0x26  -- Keyboard 9 and (
key_0 = 0x27  -- Keyboard 0 and )

key_ENTER      = 0x28  -- Keyboard Return (ENTER)
key_ESC        = 0x29  -- Keyboard ESCAPE
key_BACKSPACE  = 0x2a  -- Keyboard DELETE (Backspace)
key_TAB        = 0x2b  -- Keyboard Tab
key_SPACE      = 0x2c  -- Keyboard Spacebar
key_MINUS      = 0x2d  -- Keyboard - and _
key_EQUAL      = 0x2e  -- Keyboard = and +
key_LEFTBRACE  = 0x2f  -- Keyboard [ and {
key_RIGHTBRACE = 0x30  -- Keyboard ] and }
key_BACKSLASH  = 0x31  -- Keyboard \ and |
key_HASHTILDE  = 0x32  -- Keyboard Non-US # and ~
key_SEMICOLON  = 0x33  -- Keyboard ; and :
key_APOSTROPHE = 0x34  -- Keyboard ' and "
key_GRAVE      = 0x35  -- Keyboard ` and ~
key_COMMA      = 0x36  -- Keyboard , and <
key_DOT        = 0x37  -- Keyboard . and >
key_SLASH      = 0x38  -- Keyboard / and ?
key_CAPSLOCK   = 0x39  -- Keyboard Caps Lock

key_F1 = 0x3a  -- Keyboard F1
key_F2 = 0x3b  -- Keyboard F2
key_F3 = 0x3c  -- Keyboard F3
key_F4 = 0x3d  -- Keyboard F4
key_F5 = 0x3e  -- Keyboard F5
key_F6 = 0x3f  -- Keyboard F6
key_F7 = 0x40  -- Keyboard F7
key_F8 = 0x41  -- Keyboard F8
key_F9 = 0x42  -- Keyboard F9
key_F10 = 0x43  -- Keyboard F10
key_F11 = 0x44  -- Keyboard F11
key_F12 = 0x45  -- Keyboard F12

key_SYSRQ      = 0x46  -- Keyboard Print Screen
key_SCROLLLOCK = 0x47  -- Keyboard Scroll Lock
key_PAUSE      = 0x48  -- Keyboard Pause
key_INSERT     = 0x49  -- Keyboard Insert
key_HOME       = 0x4a  -- Keyboard Home
key_PAGEUP     = 0x4b  -- Keyboard Page Up
key_DELETE     = 0x4c  -- Keyboard Delete Forward
key_END        = 0x4d  -- Keyboard End
key_PAGEDOWN   = 0x4e  -- Keyboard Page Down
key_RIGHT      = 0x4f  -- Keyboard Right Arrow
key_LEFT       = 0x50  -- Keyboard Left Arrow
key_DOWN       = 0x51  -- Keyboard Down Arrow
key_UP         = 0x52  -- Keyboard Up Arrow

key_NUMLOCK    = 0x53  -- Keyboard Num Lock and Clear
key_KPSLASH    = 0x54  -- Keypad /
key_KPASTERISK = 0x55  -- Keypad *
key_KPMINUS    = 0x56  -- Keypad -
key_KPPLUS     = 0x57  -- Keypad +
key_KPENTER    = 0x58  -- Keypad ENTER
key_KP1        = 0x59  -- Keypad 1 and End
key_KP2        = 0x5a  -- Keypad 2 and Down Arrow
key_KP3        = 0x5b  -- Keypad 3 and PageDn
key_KP4        = 0x5c  -- Keypad 4 and Left Arrow
key_KP5        = 0x5d  -- Keypad 5
key_KP6        = 0x5e  -- Keypad 6 and Right Arrow
key_KP7        = 0x5f  -- Keypad 7 and Home
key_KP8        = 0x60  -- Keypad 8 and Up Arrow
key_KP9        = 0x61  -- Keypad 9 and Page Up
key_KP0        = 0x62  -- Keypad 0 and Insert
key_KPDOT      = 0x63  -- Keypad . and Delete

key_102ND   = 0x64  -- Keyboard Non-US \ and |
key_COMPOSE = 0x65  -- Keyboard Application
key_POWER   = 0x66  -- Keyboard Power
key_KPEQUAL = 0x67  -- Keypad =

key_F13 = 0x68  -- Keyboard F13
key_F14 = 0x69  -- Keyboard F14
key_F15 = 0x6a  -- Keyboard F15
key_F16 = 0x6b  -- Keyboard F16
key_F17 = 0x6c  -- Keyboard F17
key_F18 = 0x6d  -- Keyboard F18
key_F19 = 0x6e  -- Keyboard F19
key_F20 = 0x6f  -- Keyboard F20
key_F21 = 0x70  -- Keyboard F21
key_F22 = 0x71  -- Keyboard F22
key_F23 = 0x72  -- Keyboard F23
key_F24 = 0x73  -- Keyboard F24

key_OPEN       = 0x74  -- Keyboard Execute
key_HELP       = 0x75  -- Keyboard Help
key_PROPS      = 0x76  -- Keyboard Menu
key_FRONT      = 0x77  -- Keyboard Select
key_STOP       = 0x78  -- Keyboard Stop
key_AGAIN      = 0x79  -- Keyboard Again
key_UNDO       = 0x7a  -- Keyboard Undo
key_CUT        = 0x7b  -- Keyboard Cut
key_COPY       = 0x7c  -- Keyboard Copy
key_PASTE      = 0x7d  -- Keyboard Paste
key_FIND       = 0x7e  -- Keyboard Find
key_MUTE       = 0x7f  -- Keyboard Mute
key_VOLUMEUP   = 0x80  -- Keyboard Volume Up
key_VOLUMEDOWN = 0x81  -- Keyboard Volume Down
-- 0x82  Keyboard Locking Caps Lock
-- 0x83  Keyboard Locking Num Lock
-- 0x84  Keyboard Locking Scroll Lock
key_KPCOMMA = 0x85  -- Keypad Comma
-- 0x86  Keypad Equal Sign
key_RO               = 0x87  -- Keyboard International1
key_KATAKANAHIRAGANA = 0x88  -- Keyboard International2
key_YEN              = 0x89  -- Keyboard International3
key_HENKAN           = 0x8a  -- Keyboard International4
key_MUHENKAN         = 0x8b  -- Keyboard International5
key_KPJPCOMMA        = 0x8c  -- Keyboard International6
-- 0x8d  Keyboard International7
-- 0x8e  Keyboard International8
-- 0x8f  Keyboard International9
key_HANGEUL        = 0x90  -- Keyboard LANG1
key_HANJA          = 0x91  -- Keyboard LANG2
key_KATAKANA       = 0x92  -- Keyboard LANG3
key_HIRAGANA       = 0x93  -- Keyboard LANG4
key_ZENKAKUHANKAKU = 0x94  -- Keyboard LANG5
-- 0x95  Keyboard LANG6
-- 0x96  Keyboard LANG7
-- 0x97  Keyboard LANG8
-- 0x98  Keyboard LANG9
-- 0x99  Keyboard Alternate Erase
-- 0x9a  Keyboard SysReq/Attention
-- 0x9b  Keyboard Cancel
-- 0x9c  Keyboard Clear
-- 0x9d  Keyboard Prior
-- 0x9e  Keyboard Return
-- 0x9f  Keyboard Separator
-- 0xa0  Keyboard Out
-- 0xa1  Keyboard Oper
-- 0xa2  Keyboard Clear/Again
-- 0xa3  Keyboard CrSel/Props
-- 0xa4  Keyboard ExSel

-- 0xb0  Keypad 00
-- 0xb1  Keypad 000
-- 0xb2  Thousands Separator
-- 0xb3  Decimal Separator
-- 0xb4  Currency Unit
-- 0xb5  Currency Sub-unit
key_KPLEFTPAREN  = 0xb6  -- Keypad (
key_KPRIGHTPAREN = 0xb7  -- Keypad )
-- 0xb8  Keypad {
-- 0xb9  Keypad }
-- 0xba  Keypad Tab
-- 0xbb  Keypad Backspace
-- 0xbc  Keypad A
-- 0xbd  Keypad B
-- 0xbe  Keypad C
-- 0xbf  Keypad D
-- 0xc0  Keypad E
-- 0xc1  Keypad F
-- 0xc2  Keypad XOR
-- 0xc3  Keypad ^
-- 0xc4  Keypad %
-- 0xc5  Keypad <
-- 0xc6  Keypad >
-- 0xc7  Keypad &
-- 0xc8  Keypad &&
-- 0xc9  Keypad |
-- 0xca  Keypad ||
-- 0xcb  Keypad :
-- 0xcc  Keypad #
-- 0xcd  Keypad Space
-- 0xce  Keypad @
-- 0xcf  Keypad !
-- 0xd0  Keypad Memory Store
-- 0xd1  Keypad Memory Recall
-- 0xd2  Keypad Memory Clear
-- 0xd3  Keypad Memory Add
-- 0xd4  Keypad Memory Subtract
-- 0xd5  Keypad Memory Multiply
-- 0xd6  Keypad Memory Divide
-- 0xd7  Keypad +/-
-- 0xd8  Keypad Clear
-- 0xd9  Keypad Clear Entry
-- 0xda  Keypad Binary
-- 0xdb  Keypad Octal
-- 0xdc  Keypad Decimal
-- 0xdd  Keypad Hexadecimal

key_LEFTCTRL   = 0xe0  -- Keyboard Left Control
key_LEFTSHIFT  = 0xe1  -- Keyboard Left Shift
key_LEFTALT    = 0xe2  -- Keyboard Left Alt
key_LEFTMETA   = 0xe3  -- Keyboard Left GUI
key_RIGHTCTRL  = 0xe4  -- Keyboard Right Control
key_RIGHTSHIFT = 0xe5  -- Keyboard Right Shift
key_RIGHTALT   = 0xe6  -- Keyboard Right Alt
key_RIGHTMETA  = 0xe7  -- Keyboard Right GUI

key_MEDIA_PLAYPAUSE    = 0xe8
key_MEDIA_STOPCD       = 0xe9
key_MEDIA_PREVIOUSSONG = 0xea
key_MEDIA_NEXTSONG     = 0xeb
key_MEDIA_EJECTCD      = 0xec
key_MEDIA_VOLUMEUP     = 0xed
key_MEDIA_VOLUMEDOWN   = 0xee
key_MEDIA_MUTE         = 0xef
key_MEDIA_WWW          = 0xf0
key_MEDIA_BACK         = 0xf1
key_MEDIA_FORWARD      = 0xf2
key_MEDIA_STOP         = 0xf3
key_MEDIA_FIND         = 0xf4
key_MEDIA_SCROLLUP     = 0xf5
key_MEDIA_SCROLLDOWN   = 0xf6
key_MEDIA_EDIT         = 0xf7
key_MEDIA_SLEEP        = 0xf8
key_MEDIA_COFFEE       = 0xf9
key_MEDIA_REFRESH      = 0xfa
key_MEDIA_CALC         = 0xfb
