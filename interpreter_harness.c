// Fake API for test harness

#define HID_KEYBOARD_SC_LEFT_CONTROL 0xE0
#define SPECIAL_HID_KEYS_START 0xE7
#define BUZZER_DEFAULT_FREQ 110

typedef uint8_t hid_keycode;
typedef uint8_t logical_keycode;
#define NO_KEY 0xFF
typedef struct _kbdr {int Modifier; int KeyCode[6];} KeyboardReport_Data_t;
typedef struct _msr {uint8_t X; uint8_t Y; uint8_t Button;} MouseReport_Data_t;

#include "interpreter.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>

#define FAKE_OFFSET 100000
// fake read
int16_t serial_eeprom_read(const uint8_t* addr, uint8_t* buf, int16_t len){
	const uint8_t* real_addr = addr - FAKE_OFFSET;
	memcpy(buf, real_addr, len);
	return len;
}
int8_t serial_eeprom_errno = 1;

uint32_t uptimems(){
	static uint64_t boot_time_ms = 0ll;
	struct timeval time;
	int res = gettimeofday(&time, NULL);
	if(res == -1){
		perror("gettimeofday");
		exit(1);
	}
	uint64_t time_ms = (time.tv_sec * 1000) + (time.tv_usec / 1000);
	if(boot_time_ms == 0) boot_time_ms = time_ms;

	return (uint32_t) (time_ms - boot_time_ms);
}

#define NUM_PROGRAMS 1

const program* loaded_program;

const program* config_get_program(int i){
	if(i == 0){
		return loaded_program;
	}
	else{
		return 0;
	}
}

int main(int argc, const char** argv){
	uptimems(); // init boot time
	if(argc < 2){
		printf("Usage: interpreter <binary>\n");
		exit(1);
	}
	const char* filename = argv[1];
	struct stat s;
	if(-1 == stat(filename, &s)){
		perror("Could not open binary");
		exit(1);
	}
	int size = s.st_size;

	uint8_t* data = malloc(size);
	if(!data){
		perror("Could not allocate space for program");
		exit(1);
	}
	int fd = open(filename, O_RDONLY);
	if(fd == -1){
		perror("Could not open binary");
		exit(1);
	}
	for(uint8_t* p = data; size;){
		int r = read(fd, p, size);
		if(r == -1){
			perror("Read failure");
			exit(1);
		}
		else{
			size -= r;
			p += r;
		}
	}
	close(fd);

	const program* prog = (const program*) (data + FAKE_OFFSET);
	loaded_program = prog;

	vm_init();
	vm_start(0, 10); // 'g'

	unsigned int i = 0;
	while(1){
		vm_step_all();
		if((i++ % 5) == 0){
			KeyboardReport_Data_t r;
			vm_append_KeyboardReport(&r);
		}
	}

	printf("Program terminated\n");
}


// fake checking for keys
hid_keycode keystate_check_hid_key(hid_keycode key){
	LOG("'checking' for hid key %d, returning ", key);
	static unsigned int i = 1;
	if((i++ % 5) == 0){
		hid_keycode r = key == 0 ? 42 : key;
		LOG("%d\n", r);
		return r;
	}
	else{
		LOG("NO_KEY\n");
		return NO_KEY;
	}
}


/** PHYSICAL keys are not affected by keypad layer, while LOGICAL are. */
typedef enum _lkey_type { PHYSICAL, LOGICAL } lkey_type;

/** Checks if the argument key is down. */
uint8_t keystate_check_key(logical_keycode key, lkey_type ktype){
	LOG("'checking' for physical key %d, returning ", key);
	static unsigned int i = 1;
	if((i++ % 5) == 0){
		LOG("1 (found)\n");
		return 1;
	}
	else{
		LOG("0 (not found)\n");
		return 0;
	}
}

void buzzer_start(short s){
	LOG("Running buzzer for %d ms\n", s);
}

void buzzer_start_f(short s, uint8_t freq){
	LOG("Running buzzer at %d for %d ms\n", freq, s);
}
