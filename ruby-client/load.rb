#!/usr/bin/env ruby
require "ostruct"
require "keyboard_lib"

def to_hex(ary)
  k = 0;
  ary.collect {|x| "%.2x" % x}.inject { | acc, i | acc << ((k+=1) % 16 == 0 ? "\n" : " ") << i  }
end

def from_hex(str)
  str.split(/\s+/).map {|x| Integer(x, 16) }
end

Setting = Struct.new(:metadata, :mapping)

settings =
  STDIN.each_line
  .map(&:chomp)
  .chunk { |x| x.gsub!(/^#\s*/,"").nil? }
  .map(&:last)
  .each_slice(2)
  .map do |a, b|
    metadata = OpenStruct.new(Hash[a.map { |x| x.split(": ", 2) }])
    mapping = from_hex(b.join(" "))
    Setting.new(metadata, mapping)
  end

settings_by_id = settings.each_with_object({}){ |s, h| h[Integer(s.metadata.layout_id)] = s }

KeyboardLib.connected_keyboards.each do |kbd|
  layout_id = kbd.get_layout_id
  setting = settings_by_id[layout_id]
  next if setting.nil?
  puts "Writing mapping to keyboard layout #{layout_id}: #{kbd.product}"
  kbd.set_mapping setting.mapping
end
