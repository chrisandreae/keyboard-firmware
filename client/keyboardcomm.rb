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

USBRQ_DIR_DEVICE_TO_HOST = (1 << 7)
USBRQ_DIR_HOST_TO_DEVICE = 0

USBRQ_TYPE_CLASS  = (1 << 5)
USBRQ_TYPE_VENDOR = (2 << 5)

USBRQ_RCPT_DEVICE    = 0
USBRQ_RCPT_INTERFACE = 1
USBRQ_RCPT_ENDPOINT  = 2


class KeyboardComm
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

  SERIAL_VENDOR_PREFIX = "andreae.gen.nz:";

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
