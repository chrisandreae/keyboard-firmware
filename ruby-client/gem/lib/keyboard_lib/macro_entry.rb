class KeyboardLib::MacroEntry
  attr_accessor :key, :type, :data

  def initialize(key, type, data)
    type = type.to_sym
    unless [:program, :macro].include? type
      raise ArgumentError.new("Invalid macro type: #{type}")
    end

    @key  = key
    @type = type
    @data = data
  end

  def to_h
    {
      "key"  => key,
      "type" => type,
      "data" => data
    }
  end

  def self.macros_size(macros)
    macros.inject(2) do |a, m|
      a + ( m.type == :macro ? (2 + m.data.length) : 0 )
    end
  end

  def self.parse(h)
    unless ["data", "key", "type"] == h.keys.sort
      raise ArgumentError.new("Invalid serialized macro: #{h.inspect}")
    end

    self.new(h["key"], h["type"], h["data"])
  end
end
