// This file includes minimal headers from LUFA with comments stripped
// in order to provide sufficient type definitions and macros to use
// the same USB descriptor definitions in a V-USB project without GPL
// vs MIT license cross-contamination.

#ifndef __LUFA_COMPAT_H
#define __LUFA_COMPAT_H

#include <stddef.h>

#define ATTR_PACKED __attribute__((packed))
#include <stdint.h>

#define SWAPENDIAN_16(x)            (uint16_t)((((x) & 0xFF00) >> 8) | (((x) & 0x00FF) << 8))

#if defined(ARCH_BIG_ENDIAN) && !defined(le16_to_cpu)
#define CPU_TO_LE16(x)           SWAPENDIAN_16(x)
#elif !defined(le16_to_cpu)
#define CPU_TO_LE16(x)           x
#endif

#define EP_TYPE_INTERRUPT                  0x03

#define NO_DESCRIPTOR                     0


#define USB_CONFIG_POWER_MA(mA)           ((mA) >> 1)


#define USB_STRING_LEN(UnicodeChars)      (sizeof(USB_Descriptor_Header_t) + ((UnicodeChars) << 1))


#define VERSION_BCD(x)                    CPU_TO_LE16((((VERSION_TENS(x) << 4) | VERSION_ONES(x)) << 8) | \
													  ((VERSION_TENTHS(x) << 4) | VERSION_HUNDREDTHS(x)))

#define LANGUAGE_ID_ENG                   CPU_TO_LE16(0x0409)

#define ENDPOINT_DESCRIPTOR_DIR_IN        0x80

#define ENDPOINT_DESCRIPTOR_DIR_OUT       0x00

#define USB_CONFIG_ATTR_BUSPOWERED        0x80

#define USB_CONFIG_ATTR_SELFPOWERED       0x40

#define USB_CONFIG_ATTR_REMOTEWAKEUP      0x20

#define ENDPOINT_ATTR_NO_SYNC             (0 << 2)

#define ENDPOINT_ATTR_ASYNC               (1 << 2)

#define ENDPOINT_ATTR_ADAPTIVE            (2 << 2)

#define ENDPOINT_ATTR_SYNC                (3 << 2)

#define ENDPOINT_USAGE_DATA               (0 << 4)

#define ENDPOINT_USAGE_FEEDBACK           (1 << 4)

#define ENDPOINT_USAGE_IMPLICIT_FEEDBACK  (2 << 4)

enum USB_DescriptorTypes_t {
	DTYPE_Device                    = 0x01,
	DTYPE_Configuration             = 0x02,
	DTYPE_String                    = 0x03,
	DTYPE_Interface                 = 0x04,
	DTYPE_Endpoint                  = 0x05,
	DTYPE_DeviceQualifier           = 0x06,
	DTYPE_Other                     = 0x07,
	DTYPE_InterfacePower            = 0x08,
	DTYPE_InterfaceAssociation      = 0x0B,
	DTYPE_CSInterface               = 0x24,
	DTYPE_CSEndpoint                = 0x25,
};


enum USB_Descriptor_ClassSubclassProtocol_t {
	USB_CSCP_NoDeviceClass          = 0x00,
	USB_CSCP_NoDeviceSubclass       = 0x00,
	USB_CSCP_NoDeviceProtocol       = 0x00,
	USB_CSCP_VendorSpecificClass    = 0xFF,
	USB_CSCP_VendorSpecificSubclass = 0xFF,
	USB_CSCP_VendorSpecificProtocol = 0xFF,
	USB_CSCP_IADDeviceClass         = 0xEF,
	USB_CSCP_IADDeviceSubclass      = 0x02,
	USB_CSCP_IADDeviceProtocol      = 0x01,
};

typedef struct
{
	uint8_t Size;
	uint8_t Type;
} ATTR_PACKED USB_Descriptor_Header_t;


typedef struct
{
	USB_Descriptor_Header_t Header;

	uint16_t USBSpecification;
	uint8_t  Class;
	uint8_t  SubClass;
	uint8_t  Protocol;

	uint8_t  Endpoint0Size;

	uint16_t VendorID;
	uint16_t ProductID;
	uint16_t ReleaseNumber;

	uint8_t  ManufacturerStrIndex;
	uint8_t  ProductStrIndex;
	uint8_t  SerialNumStrIndex;
	uint8_t  NumberOfConfigurations;
} ATTR_PACKED USB_Descriptor_Device_t;

typedef struct
{
	USB_Descriptor_Header_t Header;

	uint16_t TotalConfigurationSize;
	uint8_t  TotalInterfaces;

	uint8_t  ConfigurationNumber;
	uint8_t  ConfigurationStrIndex;

	uint8_t  ConfigAttributes;

	uint8_t  MaxPowerConsumption;
} ATTR_PACKED USB_Descriptor_Configuration_Header_t;

typedef struct
{
	USB_Descriptor_Header_t Header;

	uint8_t InterfaceNumber;
	uint8_t AlternateSetting;
	uint8_t TotalEndpoints;

	uint8_t Class;
	uint8_t SubClass;
	uint8_t Protocol;

	uint8_t InterfaceStrIndex;
} ATTR_PACKED USB_Descriptor_Interface_t;

typedef struct
{
	USB_Descriptor_Header_t Header;

	uint8_t FirstInterfaceIndex;
	uint8_t TotalInterfaces;

	uint8_t Class;
	uint8_t SubClass;
	uint8_t Protocol;

	uint8_t IADStrIndex;
} ATTR_PACKED USB_Descriptor_Interface_Association_t;

typedef struct
{
	USB_Descriptor_Header_t Header;

	uint8_t  EndpointAddress;
	uint8_t  Attributes;
	uint16_t EndpointSize;
	uint8_t  PollingIntervalMS;
} ATTR_PACKED USB_Descriptor_Endpoint_t;

typedef struct
{
	USB_Descriptor_Header_t Header;

#if (ARCH == ARCH_AVR8)
	wchar_t  UnicodeString[];
#else
	uint16_t UnicodeString[];
#endif
} ATTR_PACKED USB_Descriptor_String_t;

#define VERSION_TENS(x)                   (int)((x) / 10)
#define VERSION_ONES(x)                   (int)((x) - (10 * VERSION_TENS(x)))
#define VERSION_TENTHS(x)                 (int)(((x) - (int)(x)) * 10)
#define VERSION_HUNDREDTHS(x)             (int)((((x) - (int)(x)) * 100) - (10 * VERSION_TENTHS(x)))

//////// HIDReportData ///////////

#define HID_RI_DATA_SIZE_MASK                   0x03
#define HID_RI_TYPE_MASK                        0x0C
#define HID_RI_TAG_MASK                         0xF0

#define HID_RI_TYPE_MAIN                        0x00
#define HID_RI_TYPE_GLOBAL                      0x04
#define HID_RI_TYPE_LOCAL                       0x08

#define HID_RI_DATA_BITS_0                      0x00
#define HID_RI_DATA_BITS_8                      0x01
#define HID_RI_DATA_BITS_16                     0x02
#define HID_RI_DATA_BITS_32                     0x03
#define HID_RI_DATA_BITS(DataBits)              HID_RI_DATA_BITS_ ## DataBits

#define _HID_RI_ENCODE_0(Data)
#define _HID_RI_ENCODE_8(Data)                  , (Data & 0xFF)
#define _HID_RI_ENCODE_16(Data)                 _HID_RI_ENCODE_8(Data)  _HID_RI_ENCODE_8(Data >> 8)
#define _HID_RI_ENCODE_32(Data)                 _HID_RI_ENCODE_16(Data) _HID_RI_ENCODE_16(Data >> 16)
#define _HID_RI_ENCODE(DataBits, ...)           _HID_RI_ENCODE_ ## DataBits(__VA_ARGS__)

#define _HID_RI_ENTRY(Type, Tag, DataBits, ...)							\
	(Type | Tag | HID_RI_DATA_BITS(DataBits)) _HID_RI_ENCODE(DataBits, (__VA_ARGS__))


#define HID_IOF_CONSTANT                        (1 << 0)
#define HID_IOF_DATA                            (0 << 0)
#define HID_IOF_VARIABLE                        (1 << 1)
#define HID_IOF_ARRAY                           (0 << 1)
#define HID_IOF_RELATIVE                        (1 << 2)
#define HID_IOF_ABSOLUTE                        (0 << 2)
#define HID_IOF_WRAP                            (1 << 3)
#define HID_IOF_NO_WRAP                         (0 << 3)
#define HID_IOF_NON_LINEAR                      (1 << 4)
#define HID_IOF_LINEAR                          (0 << 4)
#define HID_IOF_NO_PREFERRED_STATE              (1 << 5)
#define HID_IOF_PREFERRED_STATE                 (0 << 5)
#define HID_IOF_NULLSTATE                       (1 << 6)
#define HID_IOF_NO_NULL_POSITION                (0 << 6)
#define HID_IOF_VOLATILE                        (1 << 7)
#define HID_IOF_NON_VOLATILE                    (0 << 7)
#define HID_IOF_BUFFERED_BYTES                  (1 << 8)
#define HID_IOF_BITFIELD                        (0 << 8)


#define HID_RI_INPUT(DataBits, ...)             _HID_RI_ENTRY(HID_RI_TYPE_MAIN  , 0x80, DataBits, __VA_ARGS__)
#define HID_RI_OUTPUT(DataBits, ...)            _HID_RI_ENTRY(HID_RI_TYPE_MAIN  , 0x90, DataBits, __VA_ARGS__)
#define HID_RI_COLLECTION(DataBits, ...)        _HID_RI_ENTRY(HID_RI_TYPE_MAIN  , 0xA0, DataBits, __VA_ARGS__)
#define HID_RI_FEATURE(DataBits, ...)           _HID_RI_ENTRY(HID_RI_TYPE_MAIN  , 0xB0, DataBits, __VA_ARGS__)
#define HID_RI_END_COLLECTION(DataBits, ...)    _HID_RI_ENTRY(HID_RI_TYPE_MAIN  , 0xC0, DataBits, __VA_ARGS__)
#define HID_RI_USAGE_PAGE(DataBits, ...)        _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0x00, DataBits, __VA_ARGS__)
#define HID_RI_LOGICAL_MINIMUM(DataBits, ...)   _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0x10, DataBits, __VA_ARGS__)
#define HID_RI_LOGICAL_MAXIMUM(DataBits, ...)   _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0x20, DataBits, __VA_ARGS__)
#define HID_RI_PHYSICAL_MINIMUM(DataBits, ...)  _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0x30, DataBits, __VA_ARGS__)
#define HID_RI_PHYSICAL_MAXIMUM(DataBits, ...)  _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0x40, DataBits, __VA_ARGS__)
#define HID_RI_UNIT_EXPONENT(DataBits, ...)     _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0x50, DataBits, __VA_ARGS__)
#define HID_RI_UNIT(DataBits, ...)              _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0x60, DataBits, __VA_ARGS__)
#define HID_RI_REPORT_SIZE(DataBits, ...)       _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0x70, DataBits, __VA_ARGS__)
#define HID_RI_REPORT_ID(DataBits, ...)         _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0x80, DataBits, __VA_ARGS__)
#define HID_RI_REPORT_COUNT(DataBits, ...)      _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0x90, DataBits, __VA_ARGS__)
#define HID_RI_PUSH(DataBits, ...)              _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0xA0, DataBits, __VA_ARGS__)
#define HID_RI_POP(DataBits, ...)               _HID_RI_ENTRY(HID_RI_TYPE_GLOBAL, 0xB0, DataBits, __VA_ARGS__)
#define HID_RI_USAGE(DataBits, ...)             _HID_RI_ENTRY(HID_RI_TYPE_LOCAL , 0x00, DataBits, __VA_ARGS__)
#define HID_RI_USAGE_MINIMUM(DataBits, ...)     _HID_RI_ENTRY(HID_RI_TYPE_LOCAL , 0x10, DataBits, __VA_ARGS__)
#define HID_RI_USAGE_MAXIMUM(DataBits, ...)     _HID_RI_ENTRY(HID_RI_TYPE_LOCAL , 0x20, DataBits, __VA_ARGS__)



/////////////// HID.h


#define HID_KEYBOARD_MODIFER_LEFTCTRL                     (1 << 0)
#define HID_KEYBOARD_MODIFER_LEFTSHIFT                    (1 << 1)
#define HID_KEYBOARD_MODIFER_LEFTALT                      (1 << 2)
#define HID_KEYBOARD_MODIFER_LEFTGUI                      (1 << 3)
#define HID_KEYBOARD_MODIFER_RIGHTCTRL                    (1 << 4)
#define HID_KEYBOARD_MODIFER_RIGHTSHIFT                   (1 << 5)
#define HID_KEYBOARD_MODIFER_RIGHTALT                     (1 << 6)
#define HID_KEYBOARD_MODIFER_RIGHTGUI                     (1 << 7)


#define HID_KEYBOARD_LED_NUMLOCK                          (1 << 0)
#define HID_KEYBOARD_LED_CAPSLOCK                         (1 << 1)
#define HID_KEYBOARD_LED_SCROLLLOCK                       (1 << 2)
#define HID_KEYBOARD_LED_KATANA                           (1 << 3)


#define HID_KEYBOARD_SC_ERROR_ROLLOVER                    0x01
#define HID_KEYBOARD_SC_POST_FAIL                         0x02
#define HID_KEYBOARD_SC_ERROR_UNDEFINED                   0x03
#define HID_KEYBOARD_SC_A                                 0x04
#define HID_KEYBOARD_SC_B                                 0x05
#define HID_KEYBOARD_SC_C                                 0x06
#define HID_KEYBOARD_SC_D                                 0x07
#define HID_KEYBOARD_SC_E                                 0x08
#define HID_KEYBOARD_SC_F                                 0x09
#define HID_KEYBOARD_SC_G                                 0x0A
#define HID_KEYBOARD_SC_H                                 0x0B
#define HID_KEYBOARD_SC_I                                 0x0C
#define HID_KEYBOARD_SC_J                                 0x0D
#define HID_KEYBOARD_SC_K                                 0x0E
#define HID_KEYBOARD_SC_L                                 0x0F
#define HID_KEYBOARD_SC_M                                 0x10
#define HID_KEYBOARD_SC_N                                 0x11
#define HID_KEYBOARD_SC_O                                 0x12
#define HID_KEYBOARD_SC_P                                 0x13
#define HID_KEYBOARD_SC_Q                                 0x14
#define HID_KEYBOARD_SC_R                                 0x15
#define HID_KEYBOARD_SC_S                                 0x16
#define HID_KEYBOARD_SC_T                                 0x17
#define HID_KEYBOARD_SC_U                                 0x18
#define HID_KEYBOARD_SC_V                                 0x19
#define HID_KEYBOARD_SC_W                                 0x1A
#define HID_KEYBOARD_SC_X                                 0x1B
#define HID_KEYBOARD_SC_Y                                 0x1C
#define HID_KEYBOARD_SC_Z                                 0x1D
#define HID_KEYBOARD_SC_1_AND_EXCLAMATION                 0x1E
#define HID_KEYBOARD_SC_2_AND_AT                          0x1F
#define HID_KEYBOARD_SC_3_AND_HASHMARK                    0x20
#define HID_KEYBOARD_SC_4_AND_DOLLAR                      0x21
#define HID_KEYBOARD_SC_5_AND_PERCENTAGE                  0x22
#define HID_KEYBOARD_SC_6_AND_CARET                       0x23
#define HID_KEYBOARD_SC_7_AND_AND_AMPERSAND               0x24
#define HID_KEYBOARD_SC_8_AND_ASTERISK                    0x25
#define HID_KEYBOARD_SC_9_AND_OPENING_PARENTHESIS         0x26
#define HID_KEYBOARD_SC_0_AND_CLOSING_PARENTHESIS         0x27
#define HID_KEYBOARD_SC_ENTER                             0x28
#define HID_KEYBOARD_SC_ESCAPE                            0x29
#define HID_KEYBOARD_SC_BACKSPACE                         0x2A
#define HID_KEYBOARD_SC_TAB                               0x2B
#define HID_KEYBOARD_SC_SPACE                             0x2C
#define HID_KEYBOARD_SC_MINUS_AND_UNDERSCORE              0x2D
#define HID_KEYBOARD_SC_EQUAL_AND_PLUS                    0x2E
#define HID_KEYBOARD_SC_OPENING_BRACKET_AND_OPENING_BRACE 0x2F
#define HID_KEYBOARD_SC_CLOSING_BRACKET_AND_CLOSING_BRACE 0x30
#define HID_KEYBOARD_SC_BACKSLASH_AND_PIPE                0x31
#define HID_KEYBOARD_SC_NON_US_HASHMARK_AND_TILDE         0x32
#define HID_KEYBOARD_SC_SEMICOLON_AND_COLON               0x33
#define HID_KEYBOARD_SC_APOSTROPHE_AND_QUOTE              0x34
#define HID_KEYBOARD_SC_GRAVE_ACCENT_AND_TILDE            0x35
#define HID_KEYBOARD_SC_COMMA_AND_LESS_THAN_SIGN          0x36
#define HID_KEYBOARD_SC_DOT_AND_GREATER_THAN_SIGN         0x37
#define HID_KEYBOARD_SC_SLASH_AND_QUESTION_MARK           0x38
#define HID_KEYBOARD_SC_CAPS_LOCK                         0x39
#define HID_KEYBOARD_SC_F1                                0x3A
#define HID_KEYBOARD_SC_F2                                0x3B
#define HID_KEYBOARD_SC_F3                                0x3C
#define HID_KEYBOARD_SC_F4                                0x3D
#define HID_KEYBOARD_SC_F5                                0x3E
#define HID_KEYBOARD_SC_F6                                0x3F
#define HID_KEYBOARD_SC_F7                                0x40
#define HID_KEYBOARD_SC_F8                                0x41
#define HID_KEYBOARD_SC_F9                                0x42
#define HID_KEYBOARD_SC_F10                               0x43
#define HID_KEYBOARD_SC_F11                               0x44
#define HID_KEYBOARD_SC_F12                               0x45
#define HID_KEYBOARD_SC_PRINT_SCREEN                      0x46
#define HID_KEYBOARD_SC_SCROLL_LOCK                       0x47
#define HID_KEYBOARD_SC_PAUSE                             0x48
#define HID_KEYBOARD_SC_INSERT                            0x49
#define HID_KEYBOARD_SC_HOME                              0x4A
#define HID_KEYBOARD_SC_PAGE_UP                           0x4B
#define HID_KEYBOARD_SC_DELETE                            0x4C
#define HID_KEYBOARD_SC_END                               0x4D
#define HID_KEYBOARD_SC_PAGE_DOWN                         0x4E
#define HID_KEYBOARD_SC_RIGHT_ARROW                       0x4F
#define HID_KEYBOARD_SC_LEFT_ARROW                        0x50
#define HID_KEYBOARD_SC_DOWN_ARROW                        0x51
#define HID_KEYBOARD_SC_UP_ARROW                          0x52
#define HID_KEYBOARD_SC_NUM_LOCK                          0x53
#define HID_KEYBOARD_SC_KEYPAD_SLASH                      0x54
#define HID_KEYBOARD_SC_KEYPAD_ASTERISK                   0x55
#define HID_KEYBOARD_SC_KEYPAD_MINUS                      0x56
#define HID_KEYBOARD_SC_KEYPAD_PLUS                       0x57
#define HID_KEYBOARD_SC_KEYPAD_ENTER                      0x58
#define HID_KEYBOARD_SC_KEYPAD_1_AND_END                  0x59
#define HID_KEYBOARD_SC_KEYPAD_2_AND_DOWN_ARROW           0x5A
#define HID_KEYBOARD_SC_KEYPAD_3_AND_PAGE_DOWN            0x5B
#define HID_KEYBOARD_SC_KEYPAD_4_AND_LEFT_ARROW           0x5C
#define HID_KEYBOARD_SC_KEYPAD_5                          0x5D
#define HID_KEYBOARD_SC_KEYPAD_6_AND_RIGHT_ARROW          0x5E
#define HID_KEYBOARD_SC_KEYPAD_7_AND_HOME                 0x5F
#define HID_KEYBOARD_SC_KEYPAD_8_AND_UP_ARROW             0x60
#define HID_KEYBOARD_SC_KEYPAD_9_AND_PAGE_UP              0x61
#define HID_KEYBOARD_SC_KEYPAD_0_AND_INSERT               0x62
#define HID_KEYBOARD_SC_KEYPAD_DOT_AND_DELETE             0x63
#define HID_KEYBOARD_SC_NON_US_BACKSLASH_AND_PIPE         0x64
#define HID_KEYBOARD_SC_POWER                             0x66
#define HID_KEYBOARD_SC_EQUAL_SIGN                        0x67
#define HID_KEYBOARD_SC_F13                               0x68
#define HID_KEYBOARD_SC_F14                               0x69
#define HID_KEYBOARD_SC_F15                               0x6A
#define HID_KEYBOARD_SC_F16                               0x6B
#define HID_KEYBOARD_SC_F17                               0x6C
#define HID_KEYBOARD_SC_F18                               0x6D
#define HID_KEYBOARD_SC_F19                               0x6E
#define HID_KEYBOARD_SC_F20                               0x6F
#define HID_KEYBOARD_SC_F21                               0x70
#define HID_KEYBOARD_SC_F22                               0x71
#define HID_KEYBOARD_SC_F23                               0x72
#define HID_KEYBOARD_SC_F24                               0x73
#define HID_KEYBOARD_SC_EXECUTE                           0x74
#define HID_KEYBOARD_SC_HELP                              0x75
#define HID_KEYBOARD_SC_MANU                              0x76
#define HID_KEYBOARD_SC_SELECT                            0x77
#define HID_KEYBOARD_SC_STOP                              0x78
#define HID_KEYBOARD_SC_AGAIN                             0x79
#define HID_KEYBOARD_SC_UNDO                              0x7A
#define HID_KEYBOARD_SC_CUT                               0x7B
#define HID_KEYBOARD_SC_COPY                              0x7C
#define HID_KEYBOARD_SC_PASTE                             0x7D
#define HID_KEYBOARD_SC_FIND                              0x7E
#define HID_KEYBOARD_SC_MUTE                              0x7F
#define HID_KEYBOARD_SC_VOLUME_UP                         0x80
#define HID_KEYBOARD_SC_VOLUME_DOWN                       0x81
#define HID_KEYBOARD_SC_LOCKING_CAPS_LOCK                 0x82
#define HID_KEYBOARD_SC_LOCKING_NUM_LOCK                  0x83
#define HID_KEYBOARD_SC_LOCKING_SCROLL_LOCK               0x84
#define HID_KEYBOARD_SC_KEYPAD_COMMA                      0x85
#define HID_KEYBOARD_SC_KEYPAD_EQUAL_SIGN                 0x86
#define HID_KEYBOARD_SC_INTERNATIONAL1                    0x87
#define HID_KEYBOARD_SC_INTERNATIONAL2                    0x88
#define HID_KEYBOARD_SC_INTERNATIONAL3                    0x89
#define HID_KEYBOARD_SC_INTERNATIONAL4                    0x8A
#define HID_KEYBOARD_SC_INTERNATIONAL5                    0x8B
#define HID_KEYBOARD_SC_INTERNATIONAL6                    0x8C
#define HID_KEYBOARD_SC_INTERNATIONAL7                    0x8D
#define HID_KEYBOARD_SC_INTERNATIONAL8                    0x8E
#define HID_KEYBOARD_SC_INTERNATIONAL9                    0x8F
#define HID_KEYBOARD_SC_LANG1                             0x90
#define HID_KEYBOARD_SC_LANG2                             0x91
#define HID_KEYBOARD_SC_LANG3                             0x92
#define HID_KEYBOARD_SC_LANG4                             0x93
#define HID_KEYBOARD_SC_LANG5                             0x94
#define HID_KEYBOARD_SC_LANG6                             0x95
#define HID_KEYBOARD_SC_LANG7                             0x96
#define HID_KEYBOARD_SC_LANG8                             0x97
#define HID_KEYBOARD_SC_LANG9                             0x98
#define HID_KEYBOARD_SC_ALTERNATE_ERASE                   0x99
#define HID_KEYBOARD_SC_SISREQ                            0x9A
#define HID_KEYBOARD_SC_CANCEL                            0x9B
#define HID_KEYBOARD_SC_CLEAR                             0x9C
#define HID_KEYBOARD_SC_PRIOR                             0x9D
#define HID_KEYBOARD_SC_RETURN                            0x9E
#define HID_KEYBOARD_SC_SEPARATOR                         0x9F
#define HID_KEYBOARD_SC_OUT                               0xA0
#define HID_KEYBOARD_SC_OPER                              0xA1
#define HID_KEYBOARD_SC_CLEAR_AND_AGAIN                   0xA2
#define HID_KEYBOARD_SC_CRSEL_ANDPROPS                    0xA3
#define HID_KEYBOARD_SC_EXSEL                             0xA4
#define HID_KEYBOARD_SC_KEYPAD_00                         0xB0
#define HID_KEYBOARD_SC_KEYPAD_000                        0xB1
#define HID_KEYBOARD_SC_THOUSANDS_SEPARATOR               0xB2
#define HID_KEYBOARD_SC_DECIMAL_SEPARATOR                 0xB3
#define HID_KEYBOARD_SC_CURRENCY_UNIT                     0xB4
#define HID_KEYBOARD_SC_CURRENCY_SUB_UNIT                 0xB5
#define HID_KEYBOARD_SC_KEYPAD_OPENING_PARENTHESIS        0xB6
#define HID_KEYBOARD_SC_KEYPAD_CLOSING_PARENTHESIS        0xB7
#define HID_KEYBOARD_SC_KEYPAD_OPENING_BRACE              0xB8
#define HID_KEYBOARD_SC_KEYPAD_CLOSING_BRACE              0xB9
#define HID_KEYBOARD_SC_KEYPAD_TAB                        0xBA
#define HID_KEYBOARD_SC_KEYPAD_BACKSPACE                  0xBB
#define HID_KEYBOARD_SC_KEYPAD_A                          0xBC
#define HID_KEYBOARD_SC_KEYPAD_B                          0xBD
#define HID_KEYBOARD_SC_KEYPAD_C                          0xBE
#define HID_KEYBOARD_SC_KEYPAD_D                          0xBF
#define HID_KEYBOARD_SC_KEYPAD_E                          0xC0
#define HID_KEYBOARD_SC_KEYPAD_F                          0xC1
#define HID_KEYBOARD_SC_KEYPAD_XOR                        0xC2
#define HID_KEYBOARD_SC_KEYPAD_CARET                      0xC3
#define HID_KEYBOARD_SC_KEYPAD_PERCENTAGE                 0xC4
#define HID_KEYBOARD_SC_KEYPAD_LESS_THAN_SIGN             0xC5
#define HID_KEYBOARD_SC_KEYPAD_GREATER_THAN_SIGN          0xC6
#define HID_KEYBOARD_SC_KEYPAD_AMP                        0xC7
#define HID_KEYBOARD_SC_KEYPAD_AMP_AMP                    0xC8
#define HID_KEYBOARD_SC_KEYPAD_PIPE                       0xC9
#define HID_KEYBOARD_SC_KEYPAD_PIPE_PIPE                  0xCA
#define HID_KEYBOARD_SC_KEYPAD_COLON                      0xCB
#define HID_KEYBOARD_SC_KEYPAD_HASHMARK                   0xCC
#define HID_KEYBOARD_SC_KEYPAD_SPACE                      0xCD
#define HID_KEYBOARD_SC_KEYPAD_AT                         0xCE
#define HID_KEYBOARD_SC_KEYPAD_EXCLAMATION_SIGN           0xCF
#define HID_KEYBOARD_SC_KEYPAD_MEMORY_STORE               0xD0
#define HID_KEYBOARD_SC_KEYPAD_MEMORY_RECALL              0xD1
#define HID_KEYBOARD_SC_KEYPAD_MEMORY_CLEAR               0xD2
#define HID_KEYBOARD_SC_KEYPAD_MEMORY_ADD                 0xD3
#define HID_KEYBOARD_SC_KEYPAD_MEMORY_SUBTRACT            0xD4
#define HID_KEYBOARD_SC_KEYPAD_MEMORY_MULTIPLY            0xD5
#define HID_KEYBOARD_SC_KEYPAD_MEMORY_DIVIDE              0xD6
#define HID_KEYBOARD_SC_KEYPAD_PLUS_AND_MINUS             0xD7
#define HID_KEYBOARD_SC_KEYPAD_CLEAR                      0xD8
#define HID_KEYBOARD_SC_KEYPAD_CLEAR_ENTRY                0xD9
#define HID_KEYBOARD_SC_KEYPAD_BINARY                     0xDA
#define HID_KEYBOARD_SC_KEYPAD_OCTAL                      0xDB
#define HID_KEYBOARD_SC_KEYPAD_DECIMAL                    0xDC
#define HID_KEYBOARD_SC_KEYPAD_HEXADECIMAL                0xDD
#define HID_KEYBOARD_SC_LEFT_CONTROL                      0xE0
#define HID_KEYBOARD_SC_LEFT_SHIFT                        0xE1
#define HID_KEYBOARD_SC_LEFT_ALT                          0xE2
#define HID_KEYBOARD_SC_LEFT_GUI                          0xE3
#define HID_KEYBOARD_SC_RIGHT_CONTROL                     0xE4
#define HID_KEYBOARD_SC_RIGHT_SHIFT                       0xE5
#define HID_KEYBOARD_SC_RIGHT_ALT                         0xE6
#define HID_KEYBOARD_SC_RIGHT_GUI                         0xE7


enum HID_Descriptor_ClassSubclassProtocol_t
	{
		HID_CSCP_HIDClass             = 0x03,
		HID_CSCP_NonBootSubclass      = 0x00,
		HID_CSCP_BootSubclass         = 0x01,
		HID_CSCP_NonBootProtocol      = 0x00,
		HID_CSCP_KeyboardBootProtocol = 0x01,
		HID_CSCP_MouseBootProtocol    = 0x02,
	};


enum HID_ClassRequests_t
	{
		HID_REQ_GetReport       = 0x01,
		HID_REQ_GetIdle         = 0x02,
		HID_REQ_GetProtocol     = 0x03,
		HID_REQ_SetReport       = 0x09,
		HID_REQ_SetIdle         = 0x0A,
		HID_REQ_SetProtocol     = 0x0B,
	};


enum HID_DescriptorTypes_t
	{
		HID_DTYPE_HID           = 0x21,
		HID_DTYPE_Report        = 0x22,
	};


enum HID_ReportItemTypes_t
	{
		HID_REPORT_ITEM_In      = 0,
		HID_REPORT_ITEM_Out     = 1,
		HID_REPORT_ITEM_Feature = 2,
	};


typedef struct
{
	USB_Descriptor_Header_t Header;

	uint16_t                HIDSpec;
	uint8_t                 CountryCode;

	uint8_t                 TotalReportDescriptors;

	uint8_t                 HIDReportType;
	uint16_t                HIDReportLength;
} ATTR_PACKED USB_HID_Descriptor_HID_t;



typedef uint8_t USB_Descriptor_HIDReport_Datatype_t;


#endif // LUFA_COMPAT_H
