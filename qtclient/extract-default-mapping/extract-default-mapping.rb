#!/usr/bin/env ruby

# Quick hack to embed realistic tables in mock devices without having
# to worry about header compatibility.

# Build for the desired target, and run in the project root directory, eg:
#     ./qtclient/extract-default-mapping.rb kinesis_default_mapping | pbcopy

# Uses the `ruby-elf` gem

require 'elf'

def encode_for_c!(name, bytes)
  print "static const unsigned char #{name}[#{bytes.size}] = {\n\t"
  puts (bytes.each_slice(8).map do |slice|
    slice.map{|x| format("0x%.2x", x)}.join(", ")
  end.join(",\n\t"))
  puts "};"
end

sym = Elf::File.new('obj/hardware.o')[".symtab"]["logical_to_hid_map_default"]
STDERR.puts "Symbol found in section: #{sym.section.name}, which starts at #{sym.section.offset}"
data = File.read('obj/hardware.o', sym.size, sym.section.offset + sym.value)
encode_for_c!(ARGV[0], data.bytes)
