#!/usr/bin/env ruby
require "keyboard_lib"

target_layout = Integer(ARGV[0])

KeyboardLib.connected_keyboards.each do |kbd|
  layout_id = kbd.get_layout_id
  if layout_id == target_layout
    puts "Resetting keyboard with layout #{layout_id}"
    kbd.reset_fully
  else
    puts "Skipping keyboard with layout #{layout_id}"
  end
end
