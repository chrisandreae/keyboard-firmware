#include "storage/i2c_eeprom.h"

static uint8_t i2c_eeprom_read_byte(const uint8_t* addr){
	uint8_t b;
	i2c_eeprom_read(addr, &b, 1);
	return b;
}

// i2c_eeprom_write_step will only start a write when starting a page. If the
// caller isn't starting a page, it needs to start the write itself.
static void i2c_eeprom_start_write_if_unaligned(void* const addr){
	if(((intptr_t)addr & (EEEXT_PAGE_SIZE-1)) != 0){
		i2c_eeprom_start_write(addr);
	}
}

#define  TEMPLATE_FUNC_NAME                        Endpoint_Write_Control_SEStream_LE
#define  TEMPLATE_BUFFER_OFFSET(Length)            0
#define  TEMPLATE_BUFFER_MOVE(BufferPtr, Amount)   BufferPtr += Amount
#define  TEMPLATE_TRANSFER_BYTE(BufferPtr)         Endpoint_Write_8(i2c_eeprom_read_byte(BufferPtr))
#include "LUFA/Drivers/USB/Core/Template/Template_Endpoint_Control_W.c"

// This unfortunately has to peek into the internals enough to see the Buffer pointer and Length iterators.
#define  TEMPLATE_FUNC_NAME                      Endpoint_Read_Control_SEStream_LE
#define  TEMPLATE_BUFFER_OFFSET(Length)          ({ i2c_eeprom_start_write_if_unaligned(Buffer); 0; })
#define  TEMPLATE_BUFFER_MOVE(BufferPtr, Amount) BufferPtr += Amount
#define  TEMPLATE_TRANSFER_BYTE(BufferPtr) ({ uint8_t b = Endpoint_Read_8(); i2c_eeprom_write_step(BufferPtr, &b, 1, (Length == 1)); })
#include "LUFA/Drivers/USB/Core/Template/Template_Endpoint_Control_R.c"
