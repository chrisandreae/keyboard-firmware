HID_NAMES = ["",
             "Error_Rollover",                 # 0x01
             "Post_Fail",                      # 0x02
             "Error_Undefined",                # 0x03
             "A",                              # 0x04
             "B",                              # 0x05
             "C",                              # 0x06
             "D",                              # 0x07
             "E",                              # 0x08
             "F",                              # 0x09
             "G",                              # 0x0a
             "H",                              # 0x0b
             "I",                              # 0x0c
             "J",                              # 0x0d
             "K",                              # 0x0e
             "L",                              # 0x0f
             "M",                              # 0x10
             "N",                              # 0x11
             "O",                              # 0x12
             "P",                              # 0x13
             "Q",                              # 0x14
             "R",                              # 0x15
             "S",                              # 0x16
             "T",                              # 0x17
             "U",                              # 0x18
             "V",                              # 0x19
             "W",                              # 0x1a
             "X",                              # 0x1b
             "Y",                              # 0x1c
             "Z",                              # 0x1d
             "1",                              # 0x1e
             "2",                              # 0x1f
             "3",                              # 0x20
             "4",                              # 0x21
             "5",                              # 0x22
             "6",                              # 0x23
             "7",                              # 0x24
             "8",                              # 0x25
             "9",                              # 0x26
             "0",                              # 0x27
             "Enter",                          # 0x28
             "Esc",                            # 0x29
             "Bksp",                           # 0x2a
             "Tab",                            # 0x2b
             "Space",                          # 0x2c
             "-",                              # 0x2d
             "=",                              # 0x2e
             "[",                              # 0x2f
             "]",                              # 0x30
             "\\",                             # 0x31
             "Nonus_\#~",                      # 0x32
             ";",                              # 0x33
             "'",                              # 0x34
             "`",                              # 0x35
             ",",                              # 0x36
             ".",                              # 0x37
             "/",                              # 0x38
             "Caps",                           # 0x39
             "F1",                             # 0x3a
             "F2",                             # 0x3b
             "F3",                             # 0x3c
             "F4",                             # 0x3d
             "F5",                             # 0x3e
             "F6",                             # 0x3f
             "F7",                             # 0x40
             "F8",                             # 0x41
             "F9",                             # 0x42
             "F10",                            # 0x43
             "F11",                            # 0x44
             "F12",                            # 0x45
             "Prnt_Scrn",                      # 0x46
             "Scrl_Lock",                       # 0x47
             "Pause",                          # 0x48
             "Insert",                         # 0x49
             "Home",                           # 0x4a
             "Pgup",                           # 0x4b
             "Del",                            # 0x4c
             "End",                            # 0x4d
             "Pgdn",                           # 0x4e
             "Right",                          # 0x4f
             "Left",                           # 0x50
             "Down",                           # 0x51
             "Up",                             # 0x52
             "Num_Lck",                        # 0x53
             "Kpd/",                           # 0x54
             "Kpd*",                           # 0x55
             "Kpd-",                           # 0x56
             "Kpd+",                           # 0x57
             "Kpd_Enter",                      # 0x58
             "Kpd1_End",                       # 0x59
             "Kpd2",                           # 0x5a
             "Kpd3",                           # 0x5b
             "Kpd4",                           # 0x5c
             "Kpd5",                           # 0x5d
             "Kpd5",                           # 0x5e
             "Kpd7",                           # 0x5f
             "Kpd8",                           # 0x60
             "Kpd9",                           # 0x61
             "Kpd0",                           # 0x62
             "Kpd.",                           # 0x63
             "Nonus_\\|",                      # 0x64
             "Appli-_cation",                  # 0x65
             "Power",                          # 0x66
             "Equal_Sign",                     # 0x67
             "F13",                            # 0x68
             "F14",                            # 0x69
             "F15",                            # 0x6a
             "F16",                            # 0x6b
             "F17",                            # 0x6c
             "F18",                            # 0x6d
             "F19",                            # 0x6e
             "F20",                            # 0x6f
             "F21",                            # 0x70
             "F22",                            # 0x71
             "F23",                            # 0x72
             "F24",                            # 0x73
             "Execute",                        # 0x74
             "Help",                           # 0x75
             "Manu",                           # 0x76
             "Select",                         # 0x77
             "Stop",                           # 0x78
             "Again",                          # 0x79
             "Undo",                           # 0x7a
             "Cut",                            # 0x7b
             "Copy",                           # 0x7c
             "Paste",                          # 0x7d
             "Find",                           # 0x7e
             "Mute",                           # 0x7f
             "Volume_Up",                      # 0x80
             "Volume_Down",                    # 0x81
             "Locking_Caps_Lock",              # 0x82
             "Locking_Num_Lock",               # 0x83
             "Locking_Scroll_Lock",            # 0x84
             "Kpd_Comma",                      # 0x85
             "Kpd_Equal_Sign",                 # 0x86
             "International1",                 # 0x87
             "International2",                 # 0x88
             "International3",                 # 0x89
             "International4",                 # 0x8a
             "International5",                 # 0x8b
             "International6",                 # 0x8c
             "International7",                 # 0x8d
             "International8",                 # 0x8e
             "International9",                 # 0x8f
             "Lang1",                          # 0x90
             "Lang2",                          # 0x91
             "Lang3",                          # 0x92
             "Lang4",                          # 0x93
             "Lang5",                          # 0x94
             "Lang6",                          # 0x95
             "Lang7",                          # 0x96
             "Lang8",                          # 0x97
             "Lang9",                          # 0x98
             "Alternate_Erase",                # 0x99
             "Sisreq",                         # 0x9a
             "Cancel",                         # 0x9b
             "Clear",                          # 0x9c
             "Prior",                          # 0x9d
             "Return",                         # 0x9e
             "Separator",                      # 0x9f
             "Out",                            # 0xa0
             "Oper",                           # 0xa1
             "Clear_And_Again",                # 0xa2
             "Crsel_Andprops",                 # 0xa3
             "Exsel",                          # 0xa4
             "", "", "", "", "", "", "", "", "", "", "", #A5 To Af Reserved
             "Keypad_00",                      # 0xb0
             "Keypad_000",                     # 0xb1
             "Thousands_Separator",            # 0xb2
             "Decimal_Separator",              # 0xb3
             "Currency_Unit",                  # 0xb4
             "Currency_Sub_Unit",              # 0xb5
             "Keypad_Opening_Parenthesis",     # 0xb6
             "Keypad_Closing_Parenthesis",     # 0xb7
             "Keypad_Opening_Brace",           # 0xb8
             "Keypad_Closing_Brace",           # 0xb9
             "Keypad_Tab",                     # 0xba
             "Keypad_Backspace",               # 0xbb
             "Keypad_A",                       # 0xbc
             "Keypad_B",                       # 0xbd
             "Keypad_C",                       # 0xbe
             "Keypad_D",                       # 0xbf
             "Keypad_E",                       # 0xc0
             "Keypad_F",                       # 0xc1
             "Keypad_Xor",                     # 0xc2
             "Keypad_Caret",                   # 0xc3
             "Keypad_Percentage",              # 0xc4
             "Keypad_Less_Than_Sign",          # 0xc5
             "Keypad_Greater_Than_Sign",       # 0xc6
             "Keypad_Amp",                     # 0xc7
             "Keypad_Amp_Amp",                 # 0xc8
             "Keypad_Pipe",                    # 0xc9
             "Keypad_Pipe_Pipe",               # 0xca
             "Keypad_Colon",                   # 0xcb
             "Keypad_Hashmark",                # 0xcc
             "Keypad_Space",                   # 0xcd
             "Keypad_At",                      # 0xce
             "Keypad_Exclamation_Sign",        # 0xcf
             "Keypad_Memory_Store",            # 0xd0
             "Keypad_Memory_Recall",           # 0xd1
             "Keypad_Memory_Clear",            # 0xd2
             "Keypad_Memory_Add",              # 0xd3
             "Keypad_Memory_Subtract",         # 0xd4
             "Keypad_Memory_Multiply",         # 0xd5
             "Keypad_Memory_Divide",           # 0xd6
             "Keypad_Plus_And_Minus",          # 0xd7
             "Keypad_Clear",                   # 0xd8
             "Keypad_Clear_Entry",             # 0xd9
             "Keypad_Binary",                  # 0xda
             "Keypad_Octal",                   # 0xdb
             "Keypad_Decimal",                 # 0xdc
             "Keypad_Hexadecimal",             # 0xdd
             "", "",                           # De, Df Reserved
             "Left_Ctrl",                         # 0xe0
             "Left_Shift",                        # 0xe1
             "Left_Alt",                          # 0xe2
             "Left_Win",                          # 0xe3
             "Right_Ctrl",                         # 0xe4
             "Right_Shift",                        # 0xe5
             "Right_Alt",                          # 0xe6
             "Right_Win",                          # 0xe7
## Special Non-Hid Keys For My Keyboard
             "Mouse_1",                         #0xe8
             "Mouse_2",                         #0xe9
             "Mouse_3",                         #0xea
             "Mouse_4",                         #0xeb
             "Mouse_5",                         #0xec
             "Mouse_Up",                        #0xed
             "Mouse_Down",                      #0xee
             "Mouse_Left",                      #0xef
             "Mouse_Right",                     #0xf0
             "Prog_1",                           #0xf1
             "Prog_2",                           #0xf2
             "Prog_3",                           #0xf3
             "Prog_4",                           #0xf4
             "Prog_5",                           #0xf5
             "Prog_6",                           #0xf6
             "Macro",                          #0xf7
             "", "", "", "", "",               # F8-Fc Reserved
             "Progrm",                        # 0xfd (Non-Remappable)
             "Keypad",                         # 0xfe (Non-Remappable)
             "No_Key"                          # 0xff

            ]

SPECIAL_KEYS_START = 0xE8
SPECIAL_KEYS_NOREMAP_START = 0xFD
SPECIAL_KEYS_PROGRAM_START = 0xF1
NO_KEY = 0xFF

# HID_NAMES.each.with_index do | n, i |
  # print "#{n} : %.2x\n" % i
# end
