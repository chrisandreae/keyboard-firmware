// TODO stop copying from keyboard.c

typedef enum _vendor_request {
	READ_LAYOUT_ID,    // Which type of keyboard are we, what do the logical keycodes mean?
	READ_MAPPING_SIZE, // How many logical keycodes do we map?
	WRITE_MAPPING, READ_MAPPING,
	READ_DEFAULT_MAPPING,

	READ_NUM_PROGRAMS,  // How many program VMs do we run?
	READ_PROGRAMS_SIZE, // How much space is available for program storage?
	WRITE_PROGRAMS, READ_PROGRAMS,

	RESET_DEFAULTS,
	RESET_FULLY,

	READ_CONFIG_FLAGS,
	WRITE_CONFIG_FLAGS, // one byte: passed in wvalue

	READ_MACRO_INDEX_SIZE,
	WRITE_MACRO_INDEX, READ_MACRO_INDEX,
	READ_MACRO_STORAGE_SIZE,
	WRITE_MACRO_STORAGE, READ_MACRO_STORAGE,

	// Not guarded by preprocessor to avoid magic constants changing
	// due to configuration.
	WRITE_OATH_STORAGE, READ_OATH_STORAGE, READ_OATH_STORAGE_SIZE,
	OATH_SET_TIME,
} vendor_request;

