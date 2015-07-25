#ifndef __STORAGE_STREAM_H
#define __STORAGE_STREAM_H

#include "eeext_endpoint_stream.h"

#define Endpoint_Read_Control_StorageStream_LE(storage_type, buffer, length)  storage_read_lufa_stream  ## storage_type(buffer, length)
#define Endpoint_Write_Control_StorageStream_LE(storage_type, buffer, length) storage_write_lufa_stream ## storage_type(buffer, length)

#define storage_read_lufa_stream_avr_pgm(buffer, length)    Endpoint_Read_Control_PStream_LE(buffer, length)
#define storage_read_lufa_stream_avr_eeprom(buffer, length) Endpoint_Read_Control_EStream_LE(buffer, length)
#define storage_read_lufa_stream_i2c_eeprom(buffer, length) Endpoint_Read_Control_SEStream_LE(buffer, length)

#define storage_write_lufa_stream_avr_eeprom(buffer, length) Endpoint_Write_Control_EStream_LE(buffer, length)
#define storage_write_lufa_stream_i2c_eeprom(buffer, length) Endpoint_Write_Control_SEStream_LE(buffer, length)

#endif // __STORAGE_STREAM_H
