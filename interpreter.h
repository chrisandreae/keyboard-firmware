#ifndef __INTERPRETER_H
#define __INTERPRETER_H

#include <stdint.h>

#ifndef DEBUG // not used by interpreter debug harness
#include "keystate.h"
#endif

typedef enum __attribute__((__packed__)) _bytecode {
	// local variable store
	BSTORE   = 0,
	BSTORE_0 = 1,
	BSTORE_1,
	BSTORE_2,
	BSTORE_3,
	SSTORE   = 5,
	SSTORE_0 = 6,
	SSTORE_1,
	SSTORE_2,
	SSTORE_3,

	// local variable load
	BLOAD    = 10,
	BLOAD_0  = 11,
	BLOAD_1,
	BLOAD_2,
	BLOAD_3,
	SLOAD    = 15,
	SLOAD_0  = 16,
	SLOAD_1,
	SLOAD_2,
	SLOAD_3,

	// global variable store
	GBSTORE = 20,
	GBLOAD  = 21,
	GSSTORE = 22,
	GSLOAD  = 23,

	// immediate
	BCONST   = 24,
	BCONST_0 = 25,
	BCONST_1,
	BCONST_2,
	BCONST_3,
	SCONST   = 29,
	SCONST_0 = 30,
	SCONST_1,
	SCONST_2,
	SCONST_3,

	DUP   = 34,
	DUP2  = 35,
	POP   = 36,
	POP2  = 37,
	SWAP  = 38,

	// Arithmetic
	BADD      = 48,
	BSUBTRACT = 49,
	BMULTIPLY = 50,
	BDIVIDE   = 51,
	BMOD      = 52,
	BAND	  = 53,
	BOR		  = 54,
	BXOR	  = 55,
	BNOT	  = 56,
	BCMP	  = 57,
	BLSHIFT   = 58,
	BRSHIFT   = 59,
	SADD	  = 60,
	SSUBTRACT = 61,
	SMULTIPLY = 62,
	SDIVIDE   = 63,
	SMOD	  = 64,
	SAND	  = 65,
	SOR		  = 66,
	SXOR	  = 67,
	SNOT	  = 68,
	SCMP	  = 69,
	SLSHIFT   = 70,
	SRSHIFT   = 71,

	// Type conversion
	B2S = 72,
	S2B = 73,

	// control flow
	IFEQ   = 74,
	IFNE   = 75,
	IFLT   = 76,
	IFGT   = 77,
	IFGE   = 78,
	IFLE   = 79,
	GOTO   = 80,
	NOP    = 81,
	CALL   = 82,
	BRET   = 83,
	SRET   = 84,
	RET    = 85,
	VMEXIT = 86,

	// System calls:
	// keyboard:

	// void pressKey(byte h_keycode): causes the argument HID (not physical) key to be pressed. Does
	// not return until a report has been sent
	PRESSKEY = 87,

	// void releaseKey(byte h_keycode): causes the argument HID key to be released, if it was being
	// pressed by this program.
	RELEASEKEY = 88,

	// void checkKey(byte h_keycode): checks the current state for any key mapped to the argument
	// HID keycode, returns 1/0
	CHECKKEY = 89,

	// void checkPhysKey(byte p_keycode): checks the current state for the argument physical key. 0
	// means the the key that triggered the program. returns 1/0
	CHECKPHYSKEY = 90,

	// byte waitKey(byte key, short timeout): causes execution to be stopped until a key mapped to
	// the argument HID keycode (or 0 for any key) has been pressed, or the argument time has
	// elapsed (0 for indefinite).  Returns the mapped HID keycode of the first pressed key (not
	// necessarily the argument) or 0 if timed out.
	WAITKEY = 91,

	// like waitkey, but takes an argument physical keycode or 0 for the key that
	// triggered the program. Returns 1 if pressed or 0 if timed out.
	WAITPHYSKEY = 92,

	// general
	// void delay(short ms): causes execution to be stopped until arg1 ms elapsed
	DELAY = 95,

	// short getUptimeMS(): returns uptime in ms truncated to signed short int
	GETUPTIMEMS = 96,

	// short getuptime(): returns uptime in seconds truncated to signed short int
	GETUPTIME = 97,

	// void buzz(short time): requests that the buzzer be run for the next time ms
	BUZZ = 98,
} bytecode;

typedef int8_t vbyte;
typedef int16_t vshort;

typedef struct __attribute__((__packed__)) _method { // Lives in EEEXT
	uint8_t nargs;
	uint8_t nlocals;
	uint16_t code_offset;
} method;

typedef struct __attribute__((__packed__)) _program { // lives in EEEXT
	uint8_t nglobals;
	uint8_t nmethods;
	method methods[1]; // ...
} program;


// stack organization:
// [ globals | stackframe1 | stackdata1 | stackframe2 | stackdata2 ... ]

typedef struct __attribute__((__packed__)) _stack_frame { // stack frames live within the stack
	const bytecode* return_addr;
	vbyte* return_stack;  // TODO: save space by making these single byte offsets
	struct _stack_frame* previous_frame;
	vbyte locals[1];
} stack_frame;

#ifdef DEBUG
#define STACK_SIZE 1024 // bytes
#else
#define STACK_SIZE 48 // bytes
#endif
// globals
typedef struct __attribute__((__packed__)) _vmstate {
	enum __attribute__((__packed__)) { VMSTOPPED, VMCRASHED, VMNOPROGRAM, VMRUNNING, VMWAITREPORT, VMDELAY, VMWAITKEY, VMWAITPHYSKEY } state;
	// uptimems at which our current delay or waitkey ends
	uint32_t delay_end_ms;
	// key that we're waiting for
	hid_keycode wait_key;
	// the physical key that triggered this program
	logical_keycode trigger_lkey;

	uint8_t pressed_modifiers;
	hid_keycode pressed_keys[6];
	uint8_t pressed_key_count;

	const program* program;
	const bytecode* code;

	const bytecode* ip;

	vbyte* stack_top;
	stack_frame* current_frame; // points within stack

	vbyte stack[STACK_SIZE];
} vmstate;

/**
 * Initialize the virtual machines: load programs from eeprom memory and reset each VM to default halted state.
 */
void vm_init(void);

/**
 * Start or restart a VM that has exit or crashed
 */
uint8_t vm_start(uint8_t vm_idx, logical_keycode trigger_lkey);

/**
 * Execute the next instruction in each running VM
 */
void vm_step_all(void);

/**
 * Notify waiting virtual machines that a keyboard report has been sent
 */
void vm_report_callback(void);

/**
 * add the pressed key status of every running VM to an existing keyboard report
 */
void vm_append_KeyboardReport(KeyboardReport_Data_t* report);

#endif // __INTERPRETER_H
