# Programmable keyboard firmware
**(c) 2012-2014 Chris Andreae**

An extensively programmable USB keyboard firmware for AVR microcontrollers,
easily adaptable to new keyboard hardware.

## Hardware currently supported

 * Kinesis Contoured (Advantage, Professional, Model 110) with drop-in
   microcontroller replacement (see schematic/ directory)
 * Ergodox (on native Teensy hardware)

Can support most ATMega series AVR microcontrollers, using LUFA if native USB is
available and VUSB otherwise. The macro and program-interpreter features require
an external AT24C164 or compatible I2C EEPROM. (This EEPROM is already built in
to Kinesis Advantage/Professional hardware.)

## Features

 * Dynamically reprogrammable keyboard layout, including 'keypad layer'
   support (two independent key layouts toggled by a 'keypad' key).
 * Onboard layout profiles to save/restore up to ten separate programmed
   keyboard layouts.
 * Enhanced text macros that fully support modifier keys and can be triggered by
   any combination of up to four keys.
 * Programming, macro recording and layout save/load can be performed entirely
   on-keyboard with no additional software.
 * Also appears as USB mouse, mouse functions can be be bound to keys.
 * Built-in virtual machine interpreter for running up to six concurrent
   independent tasks.
 * Buzzer audio support (included in Kinesis hardware)
 * USB API for configuring, remapping and uploading programs to the
   keyboard. (C++ GUI client and Ruby library included.)

## Building

To build for a non-USB-capable AVR using the V-USB library:

````make -f Makefile.vusb HARDWARE_VARIANT=<variant>````

Currently supported hardware variants for non-USB AVRs are:

* ````KINESIS```` (Kinesis Advantage/Professional, ATMega32, board design in schematic/Kinesis{Advantage/Professional})
* ````KINESIS110```` (Kinesis Model 110, ATMega32, board design in schematic/KinesisModel110)

To build for a USB-capable AVR using the LUFA library:

````make -f Makefile.lufa HARDWARE_VARIANT=<variant> HAS_EXTERNAL_STORAGE={1,0}````

Currently supported hardware variants for USB AVRs are:

* ````ERGODOX```` (Ergodox, ATMega32u4 (Teensy))

## Usage

The default key layout for each hardware type can be found in the subdirectory ````layouts/````

### Enter/exit layout programming

Kinesis: ````Program + F12````

Ergodox: ````Program + P````

Press the above key combination to enter programming mode. In programming mode,
first press a source key to select the action of that key in the default
layout. Then, press a destination key to assign the selected action to. Repeat
source and destination keys as desired to continue remapping. The keypad key may
be pressed at any time to switch layers, in order to allow copying keys between
layers. The keypad and program keys may not be themselves remapped. When done,
press the above programming-mode key combination to leave programming mode.

### Layout saving/loading

* Save current layout          = ````Progrm + [1 - 0] + S````
* Load saved layout            = ````Progrm + [1 - 0] + L````
* Delete saved layout          = ````Progrm + [1 - 0] + D````

You can save up to 10 layouts, each associated with a number key. Layouts are
stored as their difference from the default layout: the more keys that are
remapped, the larger a saved layout will be. In total, there is space for 256
key remappings shared between all saved layouts.

### Reset to default layout

Kinesis: ````Program + F7````

Ergodox: ````Program + Z````

Resets the current layout to the default. Saved layouts are not affected.

### Reset all customizations

Kinesis: ````Program + LShift + F7````

Ergodox: ````Program + LShift + Z````

Resets all keyboard customizations (current layout, saved layouts, macros, programs).

### Start/finish macro recording (Note: requires EEPROM)

Kinesis: ````Program + F11````

Ergodox: ````Program + M````

Press the above key combination to enter macro recording mode. Then, press and
release a combination of up to four keys as a trigger for the macro. Then, type
the contents of the macro. Macros are dynamically sized: you can define up to 50
macros, whose size in total must be under 1022 bytes (each key press or release
in the macro consumes one byte). To finish recording the macro, press the above
macro recording key combination again.

To delete a macro, enter macro recording mode, press the macro's trigger
combination, and then immediately exit macro mode by pressing the above macro
recording key combination.

### Enable/disable key click (Note: requires buzzer)

````Program + \````

When key-click is enabled, a noise will be each time the keyboard registers a
keypress. This can be useful to learn to type without 'bottoming-out' the keys.

### VM Programs (Note: requires EEPROM)

The keyboard includes a built in virtual machine interpreter which can run up to
six concurrent programs. A program is bound to a trigger combination like a
macro. Once the combination is pressed, the program starts executing and
continues until it finishes. Programs can inspect the state of the keyboard,
cause keys to be pressed and released, and perform other functions as documented
in the section **Compiler** below. Programs must be uploaded to the keyboard
using the client software as described in the section **GUI Client** below.

## GUI Client

The keyboard also supports a GUI client which uses USB vendor commands to
reprogram the keyboard. The client provides:

 * Layout viewing, remapping and resetting
 * Macro trigger editing and macro removal (macro body editing is yet to come)
 * Program uploading

The GUI client is written in C++/QT and requires libusb. Run ````qmake```` then
````make```` in the ````qtclient/```` subdirectory to build. A Ruby library is
also provided to communicate with the keyboard, in the ````ruby-client````
subdirectory.

## Compiler and Virtual Machine

The keyboard can run small compiled programs written in a C-like language. To
build the compiler, run `cabal build` in the ````compiler/```` subdirectory. The
GHC Haskell compiler is required to build.

To compile a program:
  ````keyc -oprogram.k program.kc````

See ````compiler/examples/```` for some example programs.

A program is a set of global variable and function declarations. A function
named ````main```` must be present.  Control structures are C-like (if, while,
for, return), however pointers, arrays and goto are not present. Additionally,
the ````exit```` keyword may be used to terminate the program at any time.

Local variables are lexically scoped, and declarations may hide global variables
or local variables in enclosing scopes.

The language features two signed types, ````byte```` (8-bit) and ````short````
(16-bit). Bytes may be automatically promoted (with sign extension) to shorts,
but to truncate a short to a byte requres a cast.  As in Java, unsigned values
may be provided using hexadecimal literals, but will be treated by arithmetic as
signed. Certain library functions expect unsigned values. Bare literals are
interpreted as bytes: to specify a short literal, append a ````s````.

Programs by default are run in very small stacks (48 bytes), so unbounded
recursion is not recommended. This can be increased on larger memory devices by
changing ````STACK_SIZE```` in interpreter.h.

The system library includes the following functions:

* ````void pressKey(byte h_keycode)````
  Causes the argument (unsigned byte) HID (not physical) key to be pressed. Does
  not return until a report has been sent.

* ````void releaseKey(byte h_keycode)````
  Causes the argument (unsigned byte) HID key to be released, if it was being
  pressed by this program.

* ````void checkKey(byte h_keycode)````
  Checks the current state for any key mapped to the argument (unsigned byte) HID
  keycode, returns 1 or 0.

* ````void checkPhysKey(byte p_keycode)````
  Checks the current state for the argument (unsigned byte) physical key. An
  argument of 0 means the the key that triggered the program. Returns 1 or 0.

* ````byte waitKey(byte key, short timeout)````
  Causes execution to be stopped until a key mapped to the argument (unsigned
  byte) HID keycode (or 0 for any key) has been pressed, or the argument time
  has elapsed (0 for indefinite).  Returns the mapped HID keycode of the first
  pressed key (not necessarily the argument) or 0 if timed out.

* ````byte waitPhysKey(byte key, short timeout)````
  Like waitKey(), but takes an argument physical keycode or 0 for the key that
  triggered the program. Returns 1 if pressed or 0 if timed out.

* ````void delay(short ms)````
  Causes execution to be stopped until argument milliseconds have elapsed

* ````short getUptimeMS()````
  Returns keyboard uptime in ms truncated to a signed short int

* ````short getUptime()````
  Returns keyboard uptime in seconds truncated to signed short int

* ````void buzz(short time)````
  Runs the buzzer for the next 'time' ms

* ````void buzzAt(short time, unsigned byte freq)````
  Runs the buzzer for the next 'time' ms at frequency (1/(4e-6 * freq)) Hz

* ````void moveMouse(byte x, byte y)````
  Moves the mouse by the requested offset next time the mouse report is sent. Does
  not return until report has been sent.

* ````void pressMouseButtons(byte buttonMask)````
  Presses the mouse buttons specified by buttonMask (bits 1-5 = buttons 1-5). Does
  not return until report has been sent.

* ````void releaseMouseButtons(byte buttonMask)````
  Releases the mouse buttons specified by buttonMask (bits 1-5 = buttons 1-5) if
  they are pressed by this program. Does not return until report has been sent.
