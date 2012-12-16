require 'rexml/document'
require 'json'

require_relative "keyboardcomm"

class KeyboardModel
  attr_reader :defaultMapping, :programs_count, :programs_space, :keyboardImage, :keyLayout, :keypad
  attr_accessor :currentMapping, :programs, :macros

  class Keypad
    attr_reader :key, :layerStart, :layerSize
    def initialize(key, layerStart, layerSize)
      @key = key
      @layerStart = layerStart
      @layerSize = layerSize
    end

    def adjust_index(i, m)
      if(m && i > layerStart)
        return i + layerSize
      else
        return i
      end
    end
  end

  # attach to a particular keyboard, fetch information from it
  def initialize(comm)
    @layoutId = comm.get_layout_id
    @mappingSize = comm.get_mapping_size
    @defaultMapping = comm.get_default_mapping
    @currentMapping = comm.get_mapping

    @programs_count = comm.get_num_programs  ## this had better be 6
    @programs_space = comm.get_program_space

    @programs = comm.get_programs
    @macros = comm.get_macros

    layoutFile = File.new("layout/#{@layoutId}.xml", "r")
    xmldata = REXML::Document.new(layoutFile) # todo: use schema to avoid in-code validation, throw if not present

    keyboard = xmldata.root
    @keyboardImage = "layout/" + keyboard.attributes["image"] # validate existence

    @keyLayout = []
    keyboard.elements.each("./layout/key") do |elt|
      key = Hash.new
      elt.attributes.each { |k, v| key[k] = v }
      @keyLayout << key
    end

    keypad = keyboard.elements["./keypad"]
    if keypad != nil
      @keypad = Keypad.new(keypad.attributes["keyindex"].to_i, keypad.attributes["layerstart"].to_i, keypad.attributes["layersize"].to_i)
    end
  end

  # save the current layout and program to a file
  def save_settings(filename)
    data = {
      :layoutId => @layoutId,
      :mapping => @currentMapping,
      :programs => @programs,
      :macros => @macros
    }
    File.open(filename, "w") do |fh|
      JSON.dump(data, fh);
    end
  end

  def load_settings(filename)
    File.open(filename, "r") do |fh|
      data = JSON.load(fh)
      raise "Corrupted settings file" unless data.is_a?(Hash)
      raise "Corrupted settings file, does not include layout id" unless data.include?("layoutId")
      raise "Corrupted settings file, does not include mapping" unless data.include?("mapping")
      raise "Corrupted settings file, does not include programs" unless data.include?("programs")
      raise "Corrupted settings file, does not include macros" unless data.include?("macros")
      raise "Settings file does not match this keyboard" unless data["layoutId"] == @layoutId
      raise "Corrupted mapping data, bad length" unless data["mapping"].length == @mappingSize
      raise "Settings file contains invalid number of programs for this keyboard" unless data["programs"].length == @programs_count
      @currentMapping = data["mapping"]
      @programs = data["programs"]
      @macros = data["macros"]
    end
  end
end
