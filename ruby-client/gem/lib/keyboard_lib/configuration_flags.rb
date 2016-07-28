class KeyboardLib::ConfigurationFlags
  def initialize(args={})
    @keyBeepEnabled = args.delete(:keyBeepEnabled) || false;
  end

  def self.fromByte(b)
    self.new(:keyBeepEnabled => ((b & 0x01) > 0))
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
