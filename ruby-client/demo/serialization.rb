# frozen_string_literal: true

require 'iknow_view_models'
require 'byebug'

class OptionalFormatter
  attr_reader :formatter

  def initialize(formatter)
    @formatter = formatter
  end

  def load(json_val)
    formatter.load(json_val) unless json_val.nil?
  end

  def dump(val, json: true)
    formatter.dump(val) unless val.nil?
  end
end

class HexFormatter
  def dump(ary, json: true)
    ary.map { |x| format('%.2x', x) }.join
  end

  def load(str)
    str.scan(/../).map { |x| Integer(x, 16) }
  end
end

class SymbolFormatter
  def dump(sym, json: true)
    sym.to_s
  end

  def load(str)
    str.to_sym
  end
end

class MacroView < ViewModel::Record
  self.model_class = KeyboardLib::MacroEntry
  attribute :key, array: true
  attribute :type, format: SymbolFormatter.new
  attribute :data

  def self.for_new_model
    # MacroEntry constructor doesn't allow empty initialization
    super([], :program, [])
  end

  def serialize_data(json, serialize_context:)
    json.data do
      value = self.data
      value = HexFormatter.new.dump(data) if value.is_a?(Array)
      self.class.serialize(value, json, serialize_context: serialize_context)
    end
  end

  def deserialize_data(value, references:, deserialize_context:)
    value = HexFormatter.new.load(value) if value.is_a?(String)
    super(value, references: references, deserialize_context: deserialize_context)
  end
end

Configuration = Struct.new(:layout_id, :mapping, :programs, :macros)

class ConfigurationView < ViewModel::Record
  self.model_class = Configuration
  attribute :layout_id
  attribute :mapping, format: HexFormatter.new
  attribute :programs, format: OptionalFormatter.new(HexFormatter.new), array: true
  attribute :macros, using: MacroView, array: true
end
