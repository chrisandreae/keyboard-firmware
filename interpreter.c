#include <stdint.h>

#ifdef DEBUG

// standalone binary harness
#define LOG(x...) printf(x)
#include "interpreter_harness.c"

#else

// keyboard hardware
#define LOG(x...)
#include <string.h>
#include "keystate.h"
#include "buzzer.h"
#include "serial_eeprom.h"
#include "config.h"
#include "interpreter.h"

#endif

static vmstate vms[NUM_PROGRAMS];

static int vm_init_vm(vmstate* vm, const program* p){
	vm->state = VMSTOPPED;
	memset(vm, 0x0, sizeof(vm));
	vm->program = p;

	uint8_t nmethods;
	if(serial_eeprom_read((uint8_t*)&p->nmethods, (uint8_t*)&nmethods, 1) != 1){
		return serial_eeprom_errno;
	}

	vm->code = &((const bytecode*)p)[sizeof(program) + sizeof(method) * (nmethods - 1)];

	return 0;
}

static uint8_t vm_start_vm(vmstate* vm, logical_keycode trigger_lkey){
	if(vm->state == VMNOPROGRAM || vm->state >= VMRUNNING){
		// can't start a VM that doesn't have a program to run, and
		// choose not to -restart- a VM that is currently running.
		return 1;
	}

	vm->state = VMRUNNING;
	vm->trigger_lkey = trigger_lkey;

	// read in the program header (and first method header)
	// note that this relies on little-endian architecture.
	program p;
	if(serial_eeprom_read((uint8_t*)vm->program, (uint8_t*)&p, sizeof(program)) != sizeof(program)){
		return serial_eeprom_errno;
	}

	vm->ip = &vm->code[p.methods[0].code_offset];

	vm->current_frame = (stack_frame*)(vm->stack + p.nglobals);
	vm->current_frame->return_addr = 0;
	vm->current_frame->previous_frame = 0;

	vm->stack_top = ((vbyte*)vm->current_frame) + sizeof(stack_frame) + (p.methods[0].nlocals - 1);

	return 0;
}

void vm_init(void){
	for(uint8_t i = 0; i < NUM_PROGRAMS; ++i){
		const program* p = config_get_program(i);
		if(p){
			uint8_t r = vm_init_vm(&vms[i], p);
			if(r != 0) vms[i].state = VMNOPROGRAM; // failed to read from eeprom
		}
		 else{
			vms[i].state = VMNOPROGRAM; // Program not present
		}
	}
}

uint8_t vm_start(uint8_t idx, logical_keycode trigger_lkey){
	return vm_start_vm(&vms[idx], trigger_lkey);
}

static void vm_step(vmstate* vm);

void vm_step_all(void){
	for(uint8_t i = 0; i < NUM_PROGRAMS; ++i){
		if(vms[i].state >= VMRUNNING){
			vm_step(&vms[i]);
		}
	}
}

void vm_report_callback(void){
	for(uint8_t i = 0; i < NUM_PROGRAMS; ++i){
		if(vms[i].state == VMWAITREPORT)
			vms[i].state = VMRUNNING;
	}
}

void vm_append_KeyboardReport(KeyboardReport_Data_t* report){
	// find free slots
	uint8_t report_next;
	for(report_next = 0; report_next < 6 && report->KeyCode[report_next]; ++report_next);

	// iterate VMs and append
	for(uint8_t i = 0; i < NUM_PROGRAMS; ++i){
		if(vms[i].state < VMRUNNING) continue;

		uint8_t report_start = report_next;

		// add in modifier keys
		report->Modifier |= vms[i].pressed_modifiers;

		// as long as there are free slots, add in keys from vm that are not already there.
		for(uint8_t k = 0; report_next < 6 && k < vms[i].pressed_key_count; ++k){
			hid_keycode keycode = vms[i].pressed_keys[k];
			for(uint8_t j = 0; j < report_start; ++j){
				if(report->KeyCode[j] == keycode) goto next_vk; // already pressed; labelled-continue
			}
			report->KeyCode[report_next++] = keycode;
		next_vk:;
		}
	}
}


// Macros for reading from eeprom within VM. Assumes that VM is in
// scope as 'vm', and handles errors by setting state=VMCRASHED and
// returning.  Requires accurate (sizeof()able) pointer type in first
// argument.
#define READ_EEPROM_TO(DST, ADDR) {										\
		uint8_t __r = serial_eeprom_read((uint8_t*)(ADDR),				\
										 (uint8_t*)(DST),				\
										 sizeof(*(DST)));				\
		if(__r != sizeof(*(DST))){										\
			vm->state = VMCRASHED;										\
			return;														\
		}}																\

#define READ_EEPROM(TYPE, ADDR) ({										\
			TYPE __v;													\
			READ_EEPROM_TO(&__v, ADDR);									\
			__v;														\
		})																\

#define NEXTINSTR(vm) READ_EEPROM(bytecode, vm->ip++)
#define NEXTSHORT(vm) ({ vshort __v; READ_EEPROM_TO(&__v, vm->ip); vm->ip += 2; __v; })

// Stack manipulation macros
#define TOP_BYTE(vm) (vm->stack_top[0])
#define POP_BYTE(vm) ({ vbyte _b = TOP_BYTE(vm); vm->stack_top -= 1; _b; })
#define PUSH_BYTE(vm, val) ({ vm->stack_top += 1; TOP_BYTE(vm) = (val); })

#define TOP_SHORT(vm) ( ((vshort*)(vm->stack_top - 1))[0] )
#define POP_SHORT(vm) ({ vshort _s = TOP_SHORT(vm); vm->stack_top -= 2; _s; })
#define PUSH_SHORT(vm, val) ({ vm->stack_top += 2; TOP_SHORT(vm) = (val);  })

#define AS_SHORT(val) *((vshort*)&(val))


/** returns index in pressed_keys if pressed else NO_KEY */
static uint8_t vm_check_key(vmstate* vm, hid_keycode key){
	uint8_t r = NO_KEY;
	for(uint8_t i = 0; i < vm->pressed_key_count; ++i){
		if(vm->pressed_keys[i] == key){
			r = i;
			break;
		}
	}
	return r;
}

static void vm_do_return(vmstate* vm){
	vm->stack_top = vm->current_frame->return_stack;
	vm->ip = vm->current_frame->return_addr;
	vm->current_frame = vm->current_frame->previous_frame;
}

static uint8_t vm_if_check(bytecode instr, vbyte val){
	switch(instr){
	case IFEQ:
		return val == 0;
	case IFNE:
		return val != 0;
	case IFLT:
		return val < 0;
	case IFGT:
		return val > 0;
	case IFGE:
		return val >= 0;
	case IFLE:
		return val <= 0;
	default:
		return 0;
	}
}

#ifdef DEBUG
static const char* bytecode_name(bytecode b);
#endif

static void vm_step(vmstate* vm){
	if(vm->state < VMRUNNING){
		LOG("Tried to step halted VM");
		return;
	}

	if(vm->stack_top > &vm->stack[STACK_SIZE-2]){ // must keep enough stack to push a short
		LOG("Stack overflow!\n");
		vm->state = VMCRASHED;
		return;
	}

	switch(vm->state){
	case VMWAITREPORT: {
		LOG("VM waiting for report send\n");
		return; // nothing to do until flag is cleared
	}
	case VMWAITPHYSKEY: {
		if(vm->wait_key == 0){ vm->wait_key = vm->trigger_lkey; }
		LOG("VM waiting for physkey %d: ", vm->wait_key);
		uint8_t pressed = keystate_check_key(vm->wait_key);
		if(pressed || (vm->delay_end_ms && uptimems() > vm->delay_end_ms)){
			if(pressed){
				LOG("Wait over, pushing 1\n");
			}
			else{
				LOG("Wait timeout expired, returning 0\n");
			}
			PUSH_BYTE(vm, pressed ? 1 : 0);
			vm->delay_end_ms = 0;
			vm->state = VMRUNNING;
		}
		else{
			LOG("Not found\n");
		}
		return;
	}
	case VMWAITKEY: {
		LOG("VM waiting for key %d: ", vm->wait_key);
		hid_keycode r = keystate_check_hid_key(vm->wait_key);
		if(r != NO_KEY || (vm->delay_end_ms && uptimems() > vm->delay_end_ms)){
			if(r != NO_KEY){
				LOG("Wait over, pushing %d\n", r);
			}
			else{
				LOG("Wait timeout expired, returning 0\n");
				r = 0;
			}
			PUSH_BYTE(vm, r);
			vm->delay_end_ms = 0;
			vm->state = VMRUNNING;
		}
		else{
			LOG("not found\n");
		}
		return;
	}
	case VMDELAY: {
		if(uptimems() > vm->delay_end_ms){
			vm->delay_end_ms = 0;
			vm->state = VMRUNNING;
			break;
		}
		else{
			return;
		}
	}
	default: ;
	}

	bytecode current_instr = NEXTINSTR(vm);

	LOG("vm step: state=%d stackheight = 0x%lx (%d) bytecode = %s (%d)\n",
		vm->state, vm->stack_top - vm->stack, *vm->stack_top, bytecode_name(current_instr), current_instr);


	//(while (search-forward "case " nil t) (upcase-word 1) (forward-word 2))

	switch(current_instr){
		// local variable store
	case BSTORE:{
		vbyte local_addr = NEXTINSTR(vm);
		vm->current_frame->locals[local_addr] = POP_BYTE(vm);
		LOG("Stored to local %d\n", local_addr);
		break;
	}
	case BSTORE_0:
		vm->current_frame->locals[0] = POP_BYTE(vm);
		break;
	case BSTORE_1:
		vm->current_frame->locals[1] = POP_BYTE(vm);
		break;
	case BSTORE_2:
		vm->current_frame->locals[2] = POP_BYTE(vm);
		break;
	case BSTORE_3:
		vm->current_frame->locals[3] = POP_BYTE(vm);
		break;

	case SSTORE: {
		vbyte local_addr = NEXTINSTR(vm);
		vshort val = POP_SHORT(vm);
		AS_SHORT(vm->current_frame->locals[local_addr]) = val;
		LOG("Stored short %d to locals %d-%d\n", val, local_addr, local_addr+1);
		break;
	}
	case SSTORE_0: {
		vshort val = POP_SHORT(vm);
		AS_SHORT(vm->current_frame->locals[0]) = val;
		LOG("Stored short %d to locals 0-1\n", val);
		break;
	}
	case SSTORE_1: {
		vshort val = POP_SHORT(vm);
		AS_SHORT(vm->current_frame->locals[1]) = val;
		LOG("Stored short %d to locals 1-2\n", val);
		break;
	}
	case SSTORE_2: {
		vshort val = POP_SHORT(vm);
		AS_SHORT(vm->current_frame->locals[2]) = val;
		LOG("Stored short %d to locals 2-3\n", val);
		break;
	}
	case SSTORE_3: {
		vshort val = POP_SHORT(vm);
		AS_SHORT(vm->current_frame->locals[3]) = val;
		LOG("Stored short %d to locals 3-4\n", val);
		break;
	}

// local variable load

	case BLOAD:{
		vbyte addr = NEXTINSTR(vm);
		PUSH_BYTE(vm, vm->current_frame->locals[addr]);
		LOG("Pushed local %d: %d\n", addr, vm->current_frame->locals[addr]);
		break;
	}
	case BLOAD_0: {
		PUSH_BYTE(vm, vm->current_frame->locals[0]);
		LOG("Pushed local: %d\n", vm->current_frame->locals[0]);
		break;
	}
	case BLOAD_1: {
		PUSH_BYTE(vm, vm->current_frame->locals[1]);
		LOG("Pushed local: %d\n", vm->current_frame->locals[1]);
		break;
	}
	case BLOAD_2: {
		PUSH_BYTE(vm, vm->current_frame->locals[2]);
		LOG("Pushed local: %d\n", vm->current_frame->locals[2]);
		break;
	}
	case BLOAD_3: {
		PUSH_BYTE(vm, vm->current_frame->locals[3]);
		LOG("Pushed local: %d\n", vm->current_frame->locals[3]);
		break;
	}

	case SLOAD: {
		vbyte addr = NEXTINSTR(vm);
		vshort val = AS_SHORT(vm->current_frame->locals[addr]);
		LOG("Pushed short from local %d-%d: %d\n", addr, addr+1, val);
		PUSH_SHORT(vm, val);
		break;
	}
	case SLOAD_0: {
		vshort val = AS_SHORT(vm->current_frame->locals[0]);
		PUSH_SHORT(vm, val);
		LOG("Pushed short from local 0-1: %d\n", val);
		break;
	}
	case SLOAD_1: {
		vshort val = AS_SHORT(vm->current_frame->locals[1]);
		PUSH_SHORT(vm, val);
		LOG("Pushed short from local 1-2: %d\n", val);
		break;
	}
	case SLOAD_2: {
		vshort val = AS_SHORT(vm->current_frame->locals[2]);
		PUSH_SHORT(vm, val);
		LOG("Pushed short from local 2-3: %d\n", val);
		break;
	}
	case SLOAD_3: {
		vshort val = AS_SHORT(vm->current_frame->locals[3]);
		PUSH_SHORT(vm, val);
		LOG("Pushed short from local 3-4: %d\n", val);
		break;
	}

		// global variable store/load

	case GBSTORE: {
		vbyte addr = NEXTINSTR(vm);
		vm->stack[addr] = POP_BYTE(vm);
		LOG("Stored to global %d\n", addr);
		break;
	}
	case GSSTORE: {
		vbyte addr = NEXTINSTR(vm);
		vshort val = POP_SHORT(vm);
		AS_SHORT(vm->stack[addr]) = val;
		LOG("Stored short %d to global %d-%d\n", val, addr, addr+1);
		break;
	}
	case GBLOAD: {
		vbyte addr = NEXTINSTR(vm);
		vbyte val = vm->stack[addr];
		PUSH_BYTE(vm, val);
		LOG("Pushed %d from global %d\n", val, addr);
		break;
	}
	case GSLOAD: {
		vbyte addr = NEXTINSTR(vm);
		vshort val = AS_SHORT(vm->stack[addr]);
		PUSH_SHORT(vm, val);
		LOG("Pushed short %d from global %d-%d\n", val, addr, addr+1);
		break;
	}

		// immediate value push

	case BCONST:{
		vbyte c = NEXTINSTR(vm);
		PUSH_BYTE(vm, c);
		LOG("Pushed %d\n", c);
		break;
	}
	case BCONST_0:
		PUSH_BYTE(vm, 0);
		break;
	case BCONST_1:
		PUSH_BYTE(vm, 1);
		break;
	case BCONST_2:
		PUSH_BYTE(vm, 2);
		break;
	case BCONST_3:
		PUSH_BYTE(vm, 3);
		break;

	case SCONST: {
		vshort s = NEXTSHORT(vm);
		PUSH_SHORT(vm, s);
		LOG("Pushed short %d\n", s);
		break;
	}
	case SCONST_0:
		PUSH_SHORT(vm, 0);
		break;
	case SCONST_1:
		PUSH_SHORT(vm, 1);
		break;
	case SCONST_2:
		PUSH_SHORT(vm, 2);
		break;
	case SCONST_3:
		PUSH_SHORT(vm, 3);
		break;

		// Stack manipulation
	case DUP: {
		vbyte top = TOP_BYTE(vm);
		PUSH_BYTE(vm, top);
		break;
	}
	case DUP2: {
		vshort top = TOP_SHORT(vm);
		PUSH_SHORT(vm, top);
		LOG("Dup2 short %d\n", top);
		break;
	}

	case POP2:
		POP_BYTE(vm);
	case POP:
		POP_BYTE(vm);
		break;
	case SWAP: {
		vbyte top = TOP_BYTE(vm);
		TOP_BYTE(vm) = (&TOP_BYTE(vm))[-1];
		(&TOP_BYTE(vm))[-1] = top;
		break;
	}
		// Arithmetic

	case BADD: {
		vbyte b = POP_BYTE(vm);
		vbyte a = POP_BYTE(vm);
		LOG("%d + %d = %d\n", a, b, a+b);
		PUSH_BYTE(vm, a + b);
		break;
	}
	case BSUBTRACT: {
		vbyte b = POP_BYTE(vm);
		vbyte a = POP_BYTE(vm);
		LOG("%d - %d = %d\n", a, b, a-b);
		PUSH_BYTE(vm, a - b);
		break;
	}
	case BMULTIPLY: {
		vbyte b = POP_BYTE(vm);
		vbyte a = POP_BYTE(vm);
		LOG("%d * %d = %d\n", a, b, a*b);
		PUSH_BYTE(vm, a * b);
		break;
	}
	case BDIVIDE: {
		vbyte b = POP_BYTE(vm);
		vbyte a = POP_BYTE(vm);
		LOG("%d / %d = %d\n", a, b, a/b);
		PUSH_BYTE(vm, a / b);
		break;
	}
	case BMOD: {
		vbyte b = POP_BYTE(vm);
		vbyte a = POP_BYTE(vm);
		LOG("%d %% %d = %d\n", a, b, a%b);
		PUSH_BYTE(vm, a % b);
		break;
	}
	case BAND: {
		vbyte b = POP_BYTE(vm);
		vbyte a = POP_BYTE(vm);
		LOG("%d & %d = %d\n", a, b, a&b);
		PUSH_BYTE(vm, a & b);
		break;
	}
	case BOR: {
		vbyte b = POP_BYTE(vm);
		vbyte a = POP_BYTE(vm);
		LOG("%d | %d = %d\n", a, b, a|b);
		PUSH_BYTE(vm, a | b);
		break;
	}
	case BXOR: {
		vbyte b = POP_BYTE(vm);
		vbyte a = POP_BYTE(vm);
		LOG("%d ^ %d = %d\n", a, b, a^b);
		PUSH_BYTE(vm, a ^ b);
		break;
	}
	case BNOT: {
		vbyte x = POP_BYTE(vm);
		LOG("~%d = %d\n", x, ~x);
		PUSH_BYTE(vm, ~x);
		break;
	}
	case BCMP: {
		vbyte b = POP_BYTE(vm);
		vbyte a = POP_BYTE(vm);
		vbyte r = (a > b) ? 1 : (a == b) ? 0 : -1;
		LOG("%d <> %d = %d\n", a, b, r);
		PUSH_BYTE(vm, r);
		break;
	}
	case BLSHIFT: {
		vbyte v = POP_BYTE(vm);
		vbyte s = POP_BYTE(vm);
		LOG("%d << %d = %d\n", v, s, v << s);
		PUSH_BYTE(vm, v << s);
		break;
	}
	case BRSHIFT: {
		vbyte v = POP_BYTE(vm);
		vbyte s = POP_BYTE(vm);
		LOG("%d >> %d = %d\n", v, s, v >> s);
		PUSH_BYTE(vm, v >> s);
		break;
	}
	case SADD: {
		vshort b = POP_SHORT(vm);
		vshort a = POP_SHORT(vm);
		LOG("%d + %d = %d\n", a, b, a+b);
		PUSH_SHORT(vm, a + b);
		break;
	}
	case SSUBTRACT: {
		vshort b = POP_SHORT(vm);
		vshort a = POP_SHORT(vm);
		LOG("%d - %d = %d\n", a, b, a-b);
		PUSH_SHORT(vm, a - b);
		break;
	}
	case SMULTIPLY: {
		vshort b = POP_SHORT(vm);
		vshort a = POP_SHORT(vm);
		LOG("%d * %d = %d\n", a, b, a*b);
		PUSH_SHORT(vm, a * b);
		break;
	}
	case SDIVIDE: {
		vshort b = POP_SHORT(vm);
		vshort a = POP_SHORT(vm);
		LOG("%d / %d = %d\n", a, b, a/b);
		PUSH_SHORT(vm, a / b);
		break;
	}
	case SMOD: {
		vshort b = POP_SHORT(vm);
		vshort a = POP_SHORT(vm);
		LOG("%d %% %d = %d\n", a, b, a%b);
		PUSH_SHORT(vm, a % b);
		break;
	}
	case SAND: {
		vshort b = POP_SHORT(vm);
		vshort a = POP_SHORT(vm);
		LOG("%d & %d = %d\n", a, b, a&b);
		PUSH_SHORT(vm, a & b);
		break;
	}
	case SOR: {
		vshort b = POP_SHORT(vm);
		vshort a = POP_SHORT(vm);
		LOG("%d | %d = %d\n", a, b, a|b);
		PUSH_SHORT(vm, a | b);
		break;
	}
	case SXOR: {
		vshort b = POP_SHORT(vm);
		vshort a = POP_SHORT(vm);
		LOG("%d ^ %d = %d\n", a, b, a^b);
		PUSH_SHORT(vm, a ^ b);
		break;
	}
	case SNOT: {
		vshort x = POP_SHORT(vm);
		LOG("~%d = %d\n", x, ~x);
		PUSH_SHORT(vm, ~x);
		break;
	}
	case SCMP: {
		vshort b = POP_SHORT(vm);
		vshort a = POP_SHORT(vm);
		vbyte r = (a > b) ? 1 : (a == b) ? 0 : -1;
		LOG("%d <> %d = %d\n", a, b, r);
		PUSH_BYTE(vm, r);
		break;
	}
	case SLSHIFT: {
		vshort v = POP_SHORT(vm);
		vbyte s = POP_BYTE(vm);
		LOG("%d << %d = %d\n", v, s, v << s);
		PUSH_SHORT(vm, v << s);
		break;
	}
	case SRSHIFT: {
		vshort v = POP_SHORT(vm);
		vbyte s = POP_BYTE(vm);
		LOG("%d >> %d = %d\n", v, s, v >> s);
		PUSH_SHORT(vm, v >> s);
		break;
	}
	case B2S: {
		vbyte b = POP_BYTE(vm);
		vshort s = (vshort) b;
		LOG("(short)0x%hx = 0x%hhx\n", b, s);
		PUSH_SHORT(vm, s);
		break;
	}
	case S2B: {
		vshort s = POP_SHORT(vm);
		vshort b = (vbyte) s;
		LOG("(byte)0x%hhx = 0x%hx\n", s, b);
		PUSH_BYTE(vm, b);
		break;
	}
	case IFEQ:
	case IFNE:
	case IFLT:
	case IFGT:
	case IFGE:
	case IFLE: {
		vbyte val = POP_BYTE(vm);
		if(!vm_if_check(current_instr, val)){
			LOG("false\n");
			vm->ip += 2; // skip the offset values
			break;
		}
		else{
			LOG("true\n"); // fall through to goto
		}
	}
	case GOTO: {
		// read signed little-endian immediate value (signed => can go backwards)
		vshort offset = NEXTSHORT(vm);
		LOG("jumping %d instructions from goto\n", offset);
		vm->ip += offset - 3; // IP is 3 instructions ahead of goto
		break;
	}

	case NOP:
		break;

	case CALL:{
		vbyte methodid = NEXTINSTR(vm);
		method method;
		READ_EEPROM_TO(&method, &vm->program->methods[methodid]);
		vbyte args_tmp[method.nargs];

		LOG("Call method %d, passing %d args (reversed): [ ", methodid, method.nargs);

		// copy args
		for(vbyte* p = args_tmp + method.nargs - 1; p >= args_tmp; ){
			vbyte arg = POP_BYTE(vm);
			*p-- = arg;
			LOG("%d ", arg);
		}
		LOG("]\n");

		// create new stack frame
		vm->stack_top++;
		stack_frame* new_frame = (stack_frame*) vm->stack_top;
		new_frame->return_addr = vm->ip;
		new_frame->return_stack = vm->stack_top - 1;
		new_frame->previous_frame = vm->current_frame;
		memcpy(new_frame->locals, args_tmp, method.nargs);

		// and update the VM
		vm->current_frame = new_frame;
		vm->stack_top += sizeof(stack_frame) + method.nlocals - 1;
		vm->ip = &vm->code[method.code_offset];
		break;
	}
	case BRET: {
		if(vm->current_frame->return_addr == 0){
			LOG("Returning from main, stopping\n");
			vm->state = VMSTOPPED;
			return; // return from main
		}
		vbyte rv = POP_BYTE(vm);
		LOG("Returning %d\n", rv);
		vm_do_return(vm);
		PUSH_BYTE(vm, rv);
		break;
	}
	case SRET: {
		if(vm->current_frame->return_addr == 0){
			LOG("Returning from main, stopping\n");
			vm->state = VMSTOPPED;
			return; // return from main
		}
		vshort rv = POP_SHORT(vm);
		LOG("Returning %d\n", rv);
		vm_do_return(vm);
		PUSH_SHORT(vm, rv);
		break;
	}
	case RET: {
		if(vm->current_frame->return_addr == 0){
			LOG("Returning from main, stopping\n");
			vm->state = VMSTOPPED;
			return; // return from main
		}
		vm_do_return(vm);
		break;
	}
	case VMEXIT:
		LOG("Exit called, stopping");
		vm->state = VMSTOPPED;
		return;
	case PRESSKEY: {
		hid_keycode key = (hid_keycode) POP_BYTE(vm);
		LOG("Press Key: %d\n", key);
		if(key >= HID_KEYBOARD_SC_LEFT_CONTROL){
			vm->pressed_modifiers |= 1 << (key - HID_KEYBOARD_SC_LEFT_CONTROL);
			LOG("Pressed modifier\n");
			vm->state = VMWAITREPORT;
			break;
		}
		else{
			uint8_t foundidx = vm_check_key(vm, key);
			if(foundidx == NO_KEY){
				LOG("Adding key press: ");
				if(vm->pressed_key_count == sizeof(vm->pressed_keys)){
					LOG("Dropped, too many\n");
					break; // too many keys, drop. Because not sending the key, don't wait.
				}
				else{
					LOG("put at %d\n", vm->pressed_key_count);
					vm->pressed_keys[vm->pressed_key_count++] = key;
				}
			}
			else{
				LOG("Already pressed\n");
			}
			vm->state = VMWAITREPORT;
		}
		break;
	}
	case RELEASEKEY: {
		hid_keycode key = (hid_keycode) POP_BYTE(vm);
		LOG("Release Key: %d\n", key);
		if(key >= HID_KEYBOARD_SC_LEFT_CONTROL){
			vm->pressed_modifiers &= ~(1 << (key - HID_KEYBOARD_SC_LEFT_CONTROL));
		}
		else{
			uint8_t foundidx = vm_check_key(vm, key);
			if(foundidx != NO_KEY){
				for(int i = foundidx + 1; i < vm->pressed_key_count; ++i){
					vm->pressed_keys[i-1] = vm->pressed_keys[i];
				}
				vm->pressed_key_count--;
			}
		}
		vm->state = VMWAITREPORT;
		break;
	}
	case CHECKKEY: {
		hid_keycode key = (hid_keycode) POP_BYTE(vm);
		LOG("Check KEY: %d\n", key);
		uint8_t foundidx = keystate_check_hid_key(key);
		PUSH_BYTE(vm, (foundidx != NO_KEY));
		break;
	}
	case CHECKPHYSKEY: {
		logical_keycode lkey = (logical_keycode) POP_BYTE(vm);
		if(lkey == 0){ lkey = vm->trigger_lkey; }
		uint8_t ispressed = keystate_check_key(lkey);
		PUSH_BYTE(vm, ispressed);
		break;
	}
	case WAITKEY:
		LOG("WaitKey:");
		vm->state = VMWAITKEY;
		goto wait_rest;
	case WAITPHYSKEY:
		LOG("WaitPhysKey:");
		vm->state = VMWAITPHYSKEY;
	wait_rest: {
			vshort delay = POP_SHORT(vm);
			vm->wait_key = POP_BYTE(vm);
			LOG("%d (timeout %d)\n", vm->wait_key, delay);
			if(delay <= 0){
				vm->delay_end_ms = 0;
			}
			else{
				vm->delay_end_ms = uptimems() + delay;
			}
			break;
		}
	case DELAY: {
		vshort delay = POP_SHORT(vm);
		if(delay < 0) delay = 0;
		vm->delay_end_ms = uptimems() + delay;
		vm->state = VMDELAY;
		break;
	}
	case BUZZ: {
		vshort delay = POP_SHORT(vm);
		if(delay > 0){
			buzzer_start(delay);
		}
		break;
	}
	case GETUPTIMEMS: {
		vshort ms = uptimems() & 0x7fff;
		PUSH_SHORT(vm, ms);
		break;
	}
	case GETUPTIME: {
		vshort ms = (uptimems() / 1000) & 0x7fff;
		PUSH_SHORT(vm, ms);
		break;
	}
	}
}



#ifdef DEBUG
const char* bytecode_name(bytecode b){
	switch(b){
	case BSTORE: return "BSTORE";
	case BSTORE_0: return "BSTORE_0";
	case BSTORE_1: return "BSTORE_1";
	case BSTORE_2: return "BSTORE_2";
	case BSTORE_3: return "BSTORE_3";
	case SSTORE: return "SSTORE";
	case SSTORE_0: return "SSTORE_0";
	case SSTORE_1: return "SSTORE_1";
	case SSTORE_2: return "SSTORE_2";
	case SSTORE_3: return "SSTORE_3";
	case BLOAD: return "BLOAD";
	case BLOAD_0: return "BLOAD_0";
	case BLOAD_1: return "BLOAD_1";
	case BLOAD_2: return "BLOAD_2";
	case BLOAD_3: return "BLOAD_3";
	case SLOAD: return "SLOAD";
	case SLOAD_0: return "SLOAD_0";
	case SLOAD_1: return "SLOAD_1";
	case SLOAD_2: return "SLOAD_2";
	case SLOAD_3: return "SLOAD_3";
	case GBSTORE: return "GBSTORE";
	case GBLOAD: return "GBLOAD";
	case GSSTORE: return "GSSTORE";
	case GSLOAD: return "GSLOAD";
	case BCONST: return "BCONST";
	case BCONST_0: return "BCONST_0";
	case BCONST_1: return "BCONST_1";
	case BCONST_2: return "BCONST_2";
	case BCONST_3: return "BCONST_3";
	case SCONST: return "SCONST";
	case SCONST_0: return "SCONST_0";
	case SCONST_1: return "SCONST_1";
	case SCONST_2: return "SCONST_2";
	case SCONST_3: return "SCONST_3";
	case DUP: return "DUP";
	case DUP2: return "DUP2";
	case SWAP: return "SWAP";
	case BADD: return "BADD";
	case BSUBTRACT: return "BSUBTRACT";
	case BMULTIPLY: return "BMULTIPLY";
	case BDIVIDE: return "BDIVIDE";
	case BAND: return "BAND";
	case BOR: return "BOR";
	case BXOR: return "BXOR";
	case BCMP: return "BCMP";
	case SADD: return "SADD";
	case SSUBTRACT: return "SSUBTRACT";
	case SMULTIPLY: return "SMULTIPLY";
	case SDIVIDE: return "SDIVIDE";
	case SAND: return "SAND";
	case SOR: return "SOR";
	case SXOR: return "SXOR";
	case SCMP: return "SCMP";
	case B2S: return "B2S";
	case S2B: return "S2B";
	case IFEQ: return "IFEQ";
	case IFNE: return "IFNE";
	case IFLT: return "IFLT";
	case IFGT: return "IFGT";
	case IFGE: return "IFGE";
	case IFLE: return "IFLE";
	case GOTO: return "GOTO";
	case NOP: return "NOP";
	case CALL: return "CALL";
	case BRET: return "BRET";
	case SRET: return "SRET";
	case RET: return "RET";
	case VMEXIT: return "VMEXIT";
	case PRESSKEY: return "PRESSKEY";
	case RELEASEKEY: return "RELEASEKEY";
	case CHECKKEY: return "CHECKKEY";
	case CHECKPHYSKEY: return "CHECKPHYSKEY";
	case WAITKEY: return "WAITKEY";
	case WAITPHYSKEY: return "WAITPHYSKEY";
	case DELAY: return "DELAY";
	default: return "WAT";
	}
}
#endif
