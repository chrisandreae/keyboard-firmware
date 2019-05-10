#!/usr/bin/env ruby

require 'keyboard_lib'
require_relative './serialization.rb'

settings =
  $<.each_line
  .map(&:chomp)
  .reject { |l| l =~ /^#/ }
  .map { |l| JSON.parse(l) }
  .map { |j| ConfigurationView.deserialize_from_view(j).model }
  .index_by(&:layout_id)

KeyboardLib.connected_keyboards.each do |kbd|
  layout_id = kbd.get_layout_id
  setting = settings[layout_id]

  if setting.nil?
    puts "No settings for keyboard with layout id #{layout_id}"
    next
  end

  puts "Writing config to keyboard layout #{layout_id}: #{kbd.product}"
  kbd.set_mapping  setting.mapping
  kbd.set_programs setting.programs
  kbd.set_macros   setting.macros
end
