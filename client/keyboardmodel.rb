require 'rexml/document'

require_relative "keyboardcomm"

class KeyboardModel
  attr_reader :defaultMapping, :currentMapping, :programs_count, :programs_space, :programs, :keyboardImage, :keyLayout, :keypad
  attr_writer :currentMapping, :programs

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
end
