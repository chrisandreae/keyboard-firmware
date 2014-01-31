require_relative "keyboardcomm"
require_relative "hidcodes"
require_relative "keyboardmodel"
require_relative "keyboardview"

class KeyboardPresenter
  DEFAULT_IMAGE = "logo.png"

  def initialize
    @devices = []
    @model = nil
  end

  def showAction
    @view = KeyboardView.new(self)
    refreshAction
    Gtk.main() ## never return
  end

  def refreshAction
    @view.setStatusLine "Refreshing..." do
      @devices = KeyboardComm.enumerate
    end
    devnames = @devices.collect { |x| x.product }
    @view.updateDeviceList(devnames)
  end

  def selectDeviceAction(idx)
    if idx == -1
      # no device selected
      @model = nil

      @view.setKeyboard(DEFAULT_IMAGE, nil, nil, nil)
      @view.disableAppButtons
    else
      @view.setStatusLine "Selecting device..." do
        dev = @devices[idx]
        @kbComm = KeyboardComm.new(dev) # should check that the device still exists
        downloadSettingsAction
      end
      @view.enableAppButtons
    end
  end

  def displayCurrent
    @view.setKeyboard(@model.keyboardImage, @model.keyLayout, @model.keypad, @model.currentMapping)
    @view.setProgramSizes(@model.programs.map {|x| x == nil ? nil : x.length },
                          @model.programs_space)

    @view.setMacros(@model.macros)
  end

  def updateMacroSizes
    @view.setMacroSizes(@model.macros.length, @model.macrosMax,
                        MacroEntry.macros_size(@model.macros), @model.macros_storage_space)
  end

  def displayDefault
      @view.setKeyboard(@model.keyboardImage, @model.keyLayout, @model.keypad, @model.defaultMapping)
  end

  def resetDefaultsAction
    @model.currentMapping.replace(@model.defaultMapping)
    displayCurrent
    @view.setStatusLine "Reset mapping to defaults"
  end

  def uploadSettingsAction
    ## TODO: validation, don't upload if programs are too large.
    @view.setStatusLine "Uploading settings..."
    begin
      @kbComm.set_mapping @model.currentMapping
      @kbComm.set_programs @model.programs
      @kbComm.set_macros @model.macros
      @view.setStatusLine "Uploaded"
    rescue Exception => e
      errorDialog "Error uploading settings: #{e.message}"
      @view.setStatusLine "Upload failed"
    end
  end

  def downloadSettingsAction
    @view.setStatusLine "Downloading settings..."
    begin
      @model = KeyboardModel.new(@kbComm)
      @view.setStatusLine "Downloaded"
    rescue Exception => e
      errorDialog "Error downloading settings: #{e.message}"
      @view.setStatusLine "Download failed"
    end

    displayCurrent
  end

  def errorDialog(msg)
    md = Gtk::MessageDialog.new(@view.keyboardWindow, Gtk::Dialog::MODAL |
                                    Gtk::Dialog::DESTROY_WITH_PARENT, Gtk::MessageDialog::ERROR,
                                    Gtk::MessageDialog::BUTTONS_CLOSE, msg)
    md.run
    md.destroy
  end

  def saveSettingsAction
    dialog = Gtk::FileChooserDialog.new("Save Settings",
                                     @view.keyboardWindow,
                                     Gtk::FileChooser::ACTION_SAVE,
                                     nil,
                                     [Gtk::Stock::CANCEL, Gtk::Dialog::RESPONSE_CANCEL],
                                     [Gtk::Stock::SAVE, Gtk::Dialog::RESPONSE_ACCEPT])


    if dialog.run == Gtk::Dialog::RESPONSE_ACCEPT
      begin
        @model.save_settings(dialog.filename)
        @view.setStatusLine "Saved settings"
      rescue Exception => e
        errorDialog "Error saving settings: #{e.message}"
      end
    end
    dialog.destroy
  end

  def loadSettingsAction
    dialog = Gtk::FileChooserDialog.new("Open Settings File",
                                        @view.keyboardWindow,
                                        Gtk::FileChooser::ACTION_OPEN,
                                        nil,
                                        [Gtk::Stock::CANCEL, Gtk::Dialog::RESPONSE_CANCEL],
                                        [Gtk::Stock::OPEN, Gtk::Dialog::RESPONSE_ACCEPT])
     if dialog.run == Gtk::Dialog::RESPONSE_ACCEPT
       begin
         @model.load_settings(dialog.filename)
         @view.setStatusLine "Loaded settings"
       rescue Exception => e
         errorDialog "Error loading settings: #{e.message}"
       end
       displayCurrent
    end
    dialog.destroy
  end

  def alterProgramAction(idx)
    if @model.programs[idx] != nil
      # if there is a program, remove it
      @model.programs[idx] = nil
      displayCurrent
    else
      #open a file chooser to pick a program
      dialog = Gtk::FileChooserDialog.new("Open Program File",
                                     @view.keyboardWindow,
                                     Gtk::FileChooser::ACTION_OPEN,
                                     nil,
                                     [Gtk::Stock::CANCEL, Gtk::Dialog::RESPONSE_CANCEL],
                                     [Gtk::Stock::OPEN, Gtk::Dialog::RESPONSE_ACCEPT])


      if dialog.run == Gtk::Dialog::RESPONSE_ACCEPT
        begin
          File.open(dialog.filename) do | fh |
            programdata = fh.read.unpack("C*")
            @model.programs[idx] = programdata
          end
        rescue Exception => e
          errorDialog "Error loading program: #{e.message}"
        end
      end
      dialog.destroy
      displayCurrent
    end
  end


  def handleKeyclick(lkeyid)
  end

  def remapAction
    # Enter remap mode by switching out to a new remap presenter
    remapPres = RemapPresenter.new(self, @model, @view)
    @view.setPresenter(remapPres)
    remapPres.updateDisplay
  end

  def deleteMacroAction(macroentry)
    @model.macros.delete(macroentry)
  end

  def addNewMacroAction
    newmacro = MacroEntry.new([], :macro, [])
    @model.macros << newmacro
    newmacro
  end
end

class RemapPresenter
  def initialize(appPresenter, model, view)
    @appPresenter = appPresenter
    @model = model
    @view = view
    @source = true
  end

  def updateDisplay
    @view.enableRemapButtons(@source)
    if @source
      # TODO: indicate we're in source mode
      @view.setStatusLine("Choose source key or program")
      @appPresenter.displayDefault
    else
      @view.setStatusLine("Choose destination position")
      @appPresenter.displayCurrent
    end
  end

  def handleKeyclick(lkeyid)
    if @source
      @skey = @model.defaultMapping[lkeyid]
    else
      @model.currentMapping[lkeyid] = @skey
    end
    @source = !@source
    updateDisplay
  end

  def remapAction
    # leave remap mode, re-enable application buttons
    @appPresenter.displayCurrent
    @view.enableAppButtons
    @view.clearStatusLine
    @view.setPresenter @appPresenter
  end
end
