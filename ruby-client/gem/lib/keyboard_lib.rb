require "keyboard_lib/version"
require "keyboard_lib/comm"
require "keyboard_lib/model"

# terms of use for free usb id requires us to match the serial string
# prefix before touching the device
SERIAL_VENDOR_PREFIX = "andreae.gen.nz:"

module KeyboardLib
  def self.connected_keyboards
    usb = LIBUSB::Context.new

    devices = usb.devices.select do |dev|
      dev.serial_number.start_with? SERIAL_VENDOR_PREFIX
    end

    devices.map do |dev|
      kbd = Comm.new(dev)
    end
  end
end
