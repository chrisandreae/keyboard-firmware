#!/usr/bin/env ruby
require 'keyboard_lib'

def to_hex(ary)
  k = 0;
  ary.collect {|x| "%.2x" % x}.inject { | acc, i | acc << ((k+=1) % 16 == 0 ? "\n" : " ") << i  }
end

def from_hex(str)
  str.split(/\s+/).map {|x| Integer(x, 16) }
end

KeyboardLib.connected_keyboards.each do |kbd|

  puts "# Product: #{kbd.product}"
  puts "# Manufacturer: #{kbd.manufacturer}"
  puts "# Serial: #{kbd.serial_number}"

  puts "# layout_id: #{kbd.get_layout_id}"
  puts "# keys: #{kbd.get_mapping_size}"
  puts "# programs: #{kbd.get_num_programs}"
  puts "# program_space: #{kbd.get_program_space}"
  puts "# macro_index: #{kbd.get_macro_index_size}"
  puts "# macro_storage: #{kbd.get_macro_storage_size}"
  puts "# macro_max_keys: #{kbd.get_macro_max_keys}"
  puts "# flags: #{kbd.get_config_flags}"


  puts to_hex(kbd.get_mapping)
end
