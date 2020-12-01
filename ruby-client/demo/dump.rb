#!/usr/bin/env ruby

require 'keyboard_lib'
require 'json'
require_relative './serialization.rb'

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

  # data = {
  #   mapping: to_hex(kbd.get_mapping),
  #   programs: kbd.get_programs.map { |p| encode_bytes(p) if p },
  #   macros:  kbd.get_macros.map(&:to_h),
  # }
  # puts JSON.dump(data)
  config = Configuration.new(kbd.get_layout_id, kbd.get_mapping, kbd.get_programs, kbd.get_macros)
  view = ConfigurationView.new(config)

  puts JSON.dump(view.to_hash)
end
