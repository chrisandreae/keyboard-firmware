== Kinesis ergonomic keyboard firmware replacement ==
           == (c) 2012 Chris Andreae ==

This project aims to provide a drop-in replacement for the main
microcontroller of the Kinesis ergonomic keyboard. It has been
designed and tested with a Kinesis Professional (PS/2) keyboard, but
also functions with a Kinesis Advantage (USB) keyboard, although this
unfortunately means losing access to the built-in USB hub. It is also
designed to be easily adaptable to other keyboard hardware. See 4key.h
for a minimal example.

Additional features include:
 * Built in mouse-keys support
 * Saving and restoring programmed keyboard layouts as profiles
 * Built-in interpreter for running up to six concurrent bytecode
   programs.
 * USB API for configuring, remapping and uploading programs to
   the keyboard. (Ruby/GTK client program implemented in
   client/KeyboardClient.rb)

Not yet implemented are:
 * Macro recording

== Building ==

To build for a non-USB-capable AVR using the V-USB library:
   make -f Makefile.vusb hex

A port layout and pin mapping for the ATmega16/32 can be found in
Kinesis.h. An Eagle schematic of a sample board using this mapping can
be found in the schematic/ directory.

To build for a USB-capable AVR using the LUFA library:
   make -f Makefile.lufa

The LUFA version has not yet been updated to support the USB control
API. A port layout and description of the pin mapping to the Kinesis
board can be found in Kinesis.h for the AT90USB162 using the common
Minimus breakout board. You may need to alter these if using another
chip.

== Usage ==

The usage of the keyboard is similar to the original firmware. Key
combinations are as follows:

* Enter/exit programming   =  Progrm + F12
* Reset to default layout  =  Progrm + F7
* Reset all customizations =  Progrm + Shift + F7
* Save current layout      =  Progrm + [1 - 0] + S
* Load saved layout        =  Progrm + [1 - 0] + L
* Delete saved layout      =  Progrm + [1 - 0] + D
* Enable/disable key click =  Progrm + \

== GUI Client usage ==

The client is written in Ruby, and requires the 'libusb' and 'gtk2'
gems to be installed. Run KeyboardClient.rb in the client/
subdirectory.

== Compiler usage ==

The keyboard can run small compiled programs written in a C-like
language. To build the compiler, run 'make' in the compiler/
subdirectory. The GHC Haskell compiler is required to build.

To compile a program:
  keyc -oprogram.k program.kc

See compiler/example.kc for an example program.

A program is a set of global variable and function declarations. A
function named 'main' must be present.  Control structures are C-like
(if, while, for, return), however pointers, arrays and goto are not
present. Additionally, the 'exit' keyword may be used to terminate the
program at any time.

Local variables are lexically scoped, and declarations may
hide global variables or local variables in enclosing scopes.

The language features two signed types, byte (8-bit) and short
(16-bit). Bytes may be automatically promoted (with sign extension) to
shorts, but to truncate a short to a byte requres a cast.  As in Java,
unsigned values may be provided using hexadecimal literals, but will
be treated by arithmetic as signed. Bare literals are interpreted as
bytes: to specify a short literal, append a 's'.

Programs by default are run in very small stacks (48 bytes), so
unbounded recursion is not recommended. This can be increased on
larger memory devices by changing STACK_SIZE in interpreter.h.

The system library includes the following functions:

* void pressKey(byte h_keycode)
  Causes the argument (unsigned byte) HID (not physical) key to be
  pressed. Does not return until a report has been sent.

* void releaseKey(byte h_keycode)
  Causes the argument (unsigned byte) HID key to be released, if it
  was being pressed by this program.

* void checkKey(byte h_keycode)
  Checks the current state for any key mapped to the argument
  (unsigned byte) HID keycode, returns 1 or 0.

* void checkPhysKey(byte p_keycode)
  Checks the current state for the argument (unsigned byte) physical
  key. An argument of 0 means the the key that triggered the
  program. Returns 1 or 0.

* byte waitKey(byte key, short timeout)
  Causes execution to be stopped until a key mapped to the argument
  (unsigned byte) HID keycode (or 0 for any key) has been pressed, or
  the argument time has elapsed (0 for indefinite).  Returns the
  mapped HID keycode of the first pressed key (not necessarily the
  argument) or 0 if timed out.

* byte waitPhysKey(byte key, short timeout)
  Like waitKey(), but takes an argument physical keycode or 0 for the
  key that triggered the program. Returns 1 if pressed or 0 if timed
  out.

* void delay(short ms)
  Causes execution to be stopped until argument ms has elapsed

* short getUptimeMS()
  Returns keyboard uptime in ms truncated to a signed short int

* short getuptime()
  Returns uptime in seconds truncated to signed short int

* void buzz(short time)
  Runs the buzzer for the next 'time' ms
