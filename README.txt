== Kinesis ergonomic keyboard firmware replacement ==
           == (c) 2012 Chris Andreae ==

This project aims to provide a drop-in replacement for the main
microcontroller of the Kinesis ergonomic keyboard. It has been
designed and tested with a Kinesis Professional (PS/2) keyboard, but
would likely need only minimal adjustments to work with a Kinesis
Advantage (USB) keyboard, as its USB hardware seems to be on a
separate daughterboard. It is also designed to be easily adaptable to
other keyboard hardware. See 4key.h for a minimal example.

Additional features include:
 * Built in mouse-keys support
 * Saving and restoring programmed keyboard layouts as profiles

Not yet implemented are:
 * Macro recording
 * Key click buzzer

== Building ==

To build for a USB-capable AVR using the LUFA library:
   make -f Makefile.lufa

A port layout and description of the pin mapping to the Kinesis board
can be found in Kinesis.h for the AT90USB162 using the common Minimus
breakout board. You may need to alter these if using another chip.

To build for a non-USB-capable AVR using the V-USB library:
   make -f Makefile.vusb hex

A port layout and pin mapping for the ATmega16A can be found in
Kinesis.h. An Eagle schematic of a sample board using this mapping can
be found in the schematic/ directory.

== Usage ==

The usage of the keyboard is similar to the original firmware. Key
combinations are as follows:

* Enter/exit programming   =  Progrm + F12
* Reset to default layout  =  Progrm + F7
* Reset all customizations =  Progrm + Shift + F7
* Save current layout      =  Progrm + [1 - 0] + S
* Load saved layout        =  Progrm + [1 - 0] + L
* Delete saved layout      =  Progrm + [1 - 0] + D
