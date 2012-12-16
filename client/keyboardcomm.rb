require 'libusb'

class ConfigurationFlags
  def initialize(args={})
    @keyBeepEnabled = args.delete(:keyBeepEnabled) || false;
  end

  def self.fromByte(b)
    ConfigurationFlags.new(:keyBeepEnabled => ((b & 0x01) > 0))
  end

  def toByte()
    b = 0;
    b |= 0x01 if @keyBeepEnabled;
    b
  end

  def to_s()
    s = "("
    s << "keyBeepEnabled: " << @keyBeepEnabled.to_s
    s << ")"
    s
  end
end

class MacroEntry
  attr_accessor :key, :type, :data

  def initialize(key, type, data)
    @key = key
    @type = type
    @data = data
  end

  def self.macros_size(macros)
    macros.inject(2) do |a, m|
      a + ( m.type == :macro ? (2 + m.data.length) : 0 )
    end
  end
end

class KeyboardComm

  USBRQ_DIR_DEVICE_TO_HOST = (1 << 7)
  USBRQ_DIR_HOST_TO_DEVICE = 0

  USBRQ_TYPE_CLASS  = (1 << 5)
  USBRQ_TYPE_VENDOR = (2 << 5)

  USBRQ_RCPT_DEVICE    = 0
  USBRQ_RCPT_INTERFACE = 1
  USBRQ_RCPT_ENDPOINT  = 2

  # Vendor request constants
  VRQ_READ_LAYOUT_ID       = 0
  VRQ_READ_MAPPING_SIZE    = 1
  VRQ_WRITE_MAPPING        = 2
  VRQ_READ_MAPPING         = 3
  VRQ_READ_DEFAULT_MAPPING = 4
  VRQ_READ_NUM_PROGRAMS    = 5
  VRQ_READ_PROGRAMS_SIZE   = 6
  VRQ_WRITE_PROGRAMS       = 7
  VRQ_READ_PROGRAMS        = 8
  VRQ_RESET_DEFAULTS       = 9
  VRQ_RESET_FULLY          = 10
  VRQ_READ_CONFIG_FLAGS    = 11
  VRQ_WRITE_CONFIG_FLAGS   = 12
  VRQ_READ_MACRO_INDEX_SIZE   = 13
  VRQ_WRITE_MACRO_INDEX       = 14
  VRQ_READ_MACRO_INDEX        = 15
  VRQ_READ_MACRO_STORAGE_SIZE = 16
  VRQ_WRITE_MACRO_STORAGE     = 17
  VRQ_READ_MACRO_STORAGE      = 18
  VRQ_READ_MACRO_MAX_KEYS     = 19

  SERIAL_VENDOR_PREFIX = "andreae.gen.nz:";

  NO_KEY = 0xFF

  def self.enumerate()
    usb = LIBUSB::Context.new
    devices = usb.devices(:idVendor => 0x16c0, :idProduct => 0x27db).delete_if do |dev|
      !dev.serial_number.start_with? SERIAL_VENDOR_PREFIX
    end
    devices
  end


  def initialize(device)
    @device = device
  end

  def control_transfer(ary)
    rv = 0;
    @device.open do | handle |
      rv = handle.control_transfer(ary)
    end
    rv
  end

  def vendor_read_request(bRequest, bytes)
    control_transfer(:bmRequestType => USBRQ_DIR_DEVICE_TO_HOST | USBRQ_TYPE_VENDOR | USBRQ_RCPT_DEVICE,
                     :bRequest => bRequest,
                     :wIndex => 0,
                     :wValue => 0,
                     :dataIn => bytes)
  end

  def vendor_read_char(bRequest)
    b = vendor_read_request(bRequest, 1)
    b.unpack('C')[0]
  end

  def vendor_read_short(bRequest)
    s = vendor_read_request(bRequest, 2)
    s.unpack('S<')[0]
  end

  def vendor_write_request(bRequest, data)
    control_transfer(:bmRequestType => USBRQ_DIR_HOST_TO_DEVICE | USBRQ_TYPE_VENDOR | USBRQ_RCPT_DEVICE,
                     :bRequest => bRequest,
                     :wIndex => 0,
                     :wValue => 0,
                     :dataOut => data,
                     :timeout => 5000) # maximum usb permitted timeout
  end

  def vendor_msg_request(bRequest, wIndex, wValue)
    control_transfer(:bmRequestType => USBRQ_DIR_HOST_TO_DEVICE | USBRQ_TYPE_VENDOR | USBRQ_RCPT_DEVICE,
                     :bRequest => bRequest,
                     :wIndex => wIndex,
                     :wValue => wValue)
  end

  def set_leds(val)
    control_transfer(:bmRequestType => USBRQ_DIR_HOST_TO_DEVICE | USBRQ_TYPE_CLASS | USBRQ_RCPT_INTERFACE,
                     :bRequest => 0x09, # HID set report
                     :wIndex => 0, #keyboard
                     :wValue => 0, #unused
                     :dataOut => val.chr)
  end

  def get_layout_id()
    b = vendor_read_request(VRQ_READ_LAYOUT_ID, 1)
    b.unpack('C')[0]
  end

  def get_mapping_size()
    b = vendor_read_request(VRQ_READ_MAPPING_SIZE, 1)
    b.unpack('C')[0]
  end

  def get_num_programs()
    b = vendor_read_request(VRQ_READ_NUM_PROGRAMS, 1)
    b.unpack('C')[0]
  end

  def get_program_space_raw()
    s = vendor_read_request(VRQ_READ_PROGRAMS_SIZE, 2)
    s.unpack('S<')[0]
  end

  def get_program_space
    get_program_space_raw - (get_num_programs * 4)
  end

  def get_macro_index_size
    vendor_read_short(VRQ_READ_MACRO_INDEX_SIZE)
  end

  def get_macro_storage_size
    vendor_read_short(VRQ_READ_MACRO_STORAGE_SIZE)
  end

  def get_macro_max_keys
    vendor_read_char(VRQ_READ_MACRO_MAX_KEYS)
  end

  def get_macros
    index_sz = get_macro_index_size
    storage_sz = get_macro_storage_size
    key_len = get_macro_max_keys

    index_data = vendor_read_request(VRQ_READ_MACRO_INDEX, index_sz)
    macro_data = vendor_read_request(VRQ_READ_MACRO_STORAGE, storage_sz)
    macro_data.slice!(0, 2) # the first two bytes are the end offset, which we don't need to parse

    macros = []
    # Read index entries
    while index_data.length > 0
      entry = index_data.slice!(0, key_len+2).unpack("C#{key_len}S<")
      val = entry.pop
      key = entry
      next if key[0] == NO_KEY
      key.delete_if { |lkey| lkey == 0xff }
      # print "key: #{key} val: #{val}\n"

      macros <<
        if (val & 0x8000) > 0
          MacroEntry.new(key, :program, val & 0x7fff)
        else
          length = macro_data[val, 2].unpack("S<")[0]
          data = macro_data[val+2, length].unpack("C*")
          MacroEntry.new(key, :macro, data)
        end
    end
    macros
  end

  def set_macros(macros)
    index_sz = get_macro_index_size
    key_len = get_macro_max_keys
    index_len = index_sz / (key_len + 2)
    storage_sz = get_macro_storage_size

    # check index limits
    if macros.length > index_len
      raise "Cannot write #{macros.length} macros, device only supports #{index_len}"
    end

    if MacroEntry.macros_size(macros) > storage_sz
      raise "Cannot write #{macro_data_sz} bytes of macro data, device only supports #{storage_sz}"
    end


    # check storage limits
    macro_data_sz = macros.inject(2) do |a, m|
      a + ( m.type == :macro ? (2 + m.data.length) : 0 )
    end
    if macro_data_sz > storage_sz
      raise "Cannot write #{macro_data_sz} bytes of macro data, device only supports #{storage_sz}"
    end

    # pad and sort macro keys
    macros.each do |m|
      m.key.concat Array.new(key_len - m.key.length, 0xff)
      m.key.sort!
    end

    # sort macros by their keys
    macros.sort! { |a, b| a.key <=> b.key }

    # Iterate macros and build binary structures
    index = [].pack("C*") # force appropriate binary string encoding
    data  = [].pack("C*")
    data_len = 0

    macros.each do |m|
      index << m.key.pack("C*")
      if m.key[0] == 0xff
        # Dummy macro, store 0 in data field
        index << [0].pack("S<");
      elsif m.type == :program
        # store the program index with flag in the data field
        index << [m.data | 0x8000].pack("S<");
      elsif m.type == :macro
        # Store the macro data
        index << [data_len].pack("S<")
        data_len += m.data.length + 2
        data << [m.data.length].pack("S<")
        data << m.data.pack("C*")
      else
        raise "Unexpected error: bad macro type '#{m.type}'"
      end
    end
    data = ([data_len].pack("S<") << data);

    # Pad remainder of index
    dummy = Array.new(key_len, 0xff).push(0).pack("C#{key_len}S<")
    (index_len - macros.size).times do
      index << dummy
    end

    # print "Index:\n#{to_hex index.unpack("C*")}\n";
    # print "Data:\n#{to_hex data.unpack("C*")}\n";
    vendor_write_request(VRQ_WRITE_MACRO_INDEX, index)
    vendor_write_request(VRQ_WRITE_MACRO_STORAGE, data)
  end

  def get_macro_index_raw
    sz = get_macro_index_size
    data = vendor_read_request(VRQ_READ_MACRO_INDEX, sz)
    data.unpack("C*")
  end

  def get_macro_storage_raw
    sz = get_macro_storage_size
    data = vendor_read_request(VRQ_READ_MACRO_STORAGE, sz)
    data.unpack("C*")
  end

  def get_programs()
    sz = get_program_space_raw
    data = vendor_read_request(VRQ_READ_PROGRAMS, sz)

    indexsz = get_num_programs
    indexdata = data.slice!(0, indexsz * 4) # each member has two byte offset, two byte length
    index = indexdata.unpack("S*"); # I want to explicitly use little endian, but "S<*" doesn't work
    code = data.unpack("C*")

    programs = []
    i = 0;
    while i < index.length
      off = index[i]
      if off == 0xffff
        programs << nil
      else
        len = index[i+1]
        body = code[off, len]
        programs << body
      end
      i = i+2
    end

    return programs
  end

  def set_programs(progs)
    pmax = get_num_programs
    smax = get_program_space

    if progs.length > pmax
      raise "Cannot write #{progs.length} programs, device only supports #{pmax}"
    end

    index = []
    code = []
    for prog in progs
      if prog != nil
        off = code.length
        len = prog.length
        code.concat prog
      else
        off = 0xffff
        len = 0xffff
      end
      index << off << len
    end

    # check code size
    if code.length > smax
      raise "Cannot write #{code.length} bytes of programs, only #{smax} available"
    end

    # pad index
    if index.length < pmax * 2
      index.concat Array.new((pmax * 2) - index.length, 0xffff)
    end

    # write out
    data = index.pack("S*") + code.pack("C*") # fixme: use explicitly little-endian shorts

    # print "About to send %d bytes of program\n" % data.length
    vendor_write_request(VRQ_WRITE_PROGRAMS, data)
  end

  def get_mapping()
    sz = get_mapping_size
    s = vendor_read_request(VRQ_READ_MAPPING, sz)
    s.unpack("C*")
  end

  def get_default_mapping()
    sz = get_mapping_size
    s = vendor_read_request(VRQ_READ_DEFAULT_MAPPING, sz)
    s.unpack("C*")
  end

  ## Uploads an array of mapping_size USB keycodes. Clips if too large.
  def set_mapping(mapping)
    mapping = mapping[0, get_mapping_size]
    data = mapping.pack('C*')

    vendor_write_request(VRQ_WRITE_MAPPING, data)
    # writing to internal eeprom is really slow: 8.5ms per byte. This means 68ms per 8 byte chunk.
    # The USB requirements are very relaxed though:
    # Each data packet must be sent withing 500ms of the setup/previous data packet
    # The status stage must complete within 50ms of the last data packet
  end

  def reset()
    vendor_msg_request(VRQ_RESET_DEFAULTS, 0, 0)
  end

  def reset_fully()
    vendor_msg_request(VRQ_RESET_FULLY, 0, 0)
  end

  def get_config_flags()
    b = vendor_read_request(VRQ_READ_CONFIG_FLAGS, 1)
    ConfigurationFlags.fromByte(b.unpack("C")[0])
  end

  def set_config_flags(flags)
    vendor_msg_request(VRQ_WRITE_CONFIG_FLAGS, 0, flags.toByte);
  end

  private :control_transfer, :vendor_read_request, :vendor_write_request, :vendor_msg_request
end
