require 'json'
require 'keyboard_lib/comm'
require 'keyboard_lib/macro_entry'

class KeyboardLib::Model
  attr_reader :comm, :default_mapping, :programs_count, :programs_space, :macros_max_trigger_keys, :macros_index_space, :macros_storage_space
  attr_accessor :current_mapping, :programs, :macros

  # attach to a particular keyboard, fetch information from it
  def initialize(comm)
    @comm = comm
    read_from_keyboard
  end

  def read_from_keyboard
    @layoutId = comm.get_layout_id
    @mappingSize = comm.get_mapping_size
    @default_mapping = comm.get_default_mapping
    @current_mapping = comm.get_mapping

    @programs_count = comm.get_num_programs
    @programs_space = comm.get_program_space

    @programs = comm.get_programs
    @macros   = comm.get_macros

    @macros_index_space      = comm.get_macro_index_size
    @macros_storage_space    = comm.get_macro_storage_size
    @macros_max_trigger_keys = comm.get_macro_max_keys
  end

  def write_to_keyboard
    comm.set_mapping(current_mapping)
    comm.set_programs(programs)
    comm.set_macros(macros)
  end

  # save the current layout and program to a file
  def save_settings(filename)
    data = {
      layoutId: @layoutId,
      mapping:  @current_mapping,
      programs: @programs,
      macros:   @macros.map(&:to_h)
    }

    File.open(filename, "w") do |fh|
      JSON.dump(data, fh);
    end
  end

  def load_settings(filename)
    File.open(filename, "r") do |fh|
      data = JSON.load(fh)
      raise "Corrupted settings file" unless data.is_a?(Hash)
      raise "Corrupted settings file, does not include layout id"                 unless data.include?("layoutId")
      raise "Corrupted settings file, does not include mapping"                   unless data.include?("mapping")
      raise "Corrupted settings file, does not include programs"                  unless data.include?("programs")
      raise "Corrupted settings file, does not include macros"                    unless data.include?("macros")
      raise "Settings file does not match this keyboard"                          unless data["layoutId"]        == @layoutId
      raise "Corrupted mapping data, bad length"                                  unless data["mapping"].length  == @mappingSize
      raise "Settings file contains invalid number of programs for this keyboard" unless data["programs"].length == @programs_count

      @current_mapping = data["mapping"]
      @programs       = data["programs"]
      @macros         = data["macros"].map { |m| KeyboardLib::MacroEntry.parse(m) }
    end
  end
end
