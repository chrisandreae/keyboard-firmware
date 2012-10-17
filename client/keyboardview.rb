require 'gtk2'

require_relative "keyboardcomm"
require_relative "hidcodes"
require_relative "keyboardmodel"
require_relative "keyboardpresenter"

class KeyboardView
  attr :glade
  attr_reader :window

  def initialize(presenter)
    @presenter = presenter

    Gtk.init
    builder = Gtk::Builder::new
    builder.add_from_file("gui.xml")
    builder.connect_signals{ |handler| method(handler) }
    @window = builder.get_object("window1")

    @refreshButton = builder.get_object("refresh")

    @devCombo = builder.get_object("devices")
    @devList = Gtk::ListStore.new(String);
    @devCombo.model= @devList
    comboRenderer = Gtk::CellRendererText.new
    @devCombo.pack_start(comboRenderer, true)
    @devCombo.add_attribute(comboRenderer, 'text', 0)

    @remapButton = builder.get_object("remap")

    # TODO: rather than fixed program buttons, add them programmatically
    # in response to the number of programs reported by the keyboard.
    @programButtons = (1..6).to_a.map! do |x|
      builder.get_object("program#{x}")
    end

    @programLabels = (1..6).to_a.map! do |x|
      builder.get_object("program#{x}label")
    end
    @programSummaryLabel = builder.get_object("programlabel")

    @appWidgets = @programButtons +
      ["defaults", "remap", "download", "upload", "menusave", "menuopen"].map! { |l|builder.get_object(l) }

    @keyboardDrawing = builder.get_object "keyboardDrawing"

    @statusbar = builder.get_object "statusbar"

    # Representation of the keyboard we're drawing - set by @setKeyboard
    @layout = nil
    @keypad = nil
    @mapping = nil

    # The view keeps track of whether it is in a keypad mode, and uses this
    # to decide which part of the provided mapping to display on the picture
    @keypadMode = false

    @window.show()
  end


  ## Presenter interface
  def setPresenter(p)
    @presenter = p
  end

  def setKeyboard(filename, layout, keypad, mapping)
    @picture = Gdk::Pixbuf.new(filename)
    # layout and keypad describe location of keys and how they relate to mapping
    @layout = layout
    @keypad = keypad
    @mapping = mapping
    @keyboardDrawing.queue_draw
  end

  def setProgramSizes(sizes, space)
    sizes.each.with_index do | s, i |
      @programLabels[i].text = s == nil ? "N/A" : ("%d bytes" % s)
    end
    vsizes = sizes.select { |a| a != nil }
    count = vsizes.length
    total = vsizes.inject(0, &:+)
    @programSummaryLabel.text = "#{count} program(s): #{total}/#{space} bytes used"
  end

  def updateDeviceList(names)
    @devList.clear

    names.each do |devname|
      row = @devList.append
      row[0] = devname
    end

    if names.length > 0
      @devCombo.active = 0
    else
      @devCombo.active = -1
    end
  end

  def enableAppButtons
    @refreshButton.set_sensitive(true)
    @devCombo.set_sensitive(true)
    @remapButton.label = "Remap"
    @appWidgets.each { |b| b.set_sensitive(true) }
  end

  def disableAppButtons
    @appWidgets.each { |b| b.set_sensitive(false) }
  end

  def enableRemapButtons(source)
    disableAppButtons
    @refreshButton.set_sensitive(false)
    @devCombo.set_sensitive(false)
    @remapButton.set_sensitive(true)
    @remapButton.label = "End Remap"
    @programButtons.each { |b| b.set_sensitive(source) }
  end

  def setStatusLine(str)
    id = @statusbar.get_context_id("statusbar")
    @statusbar.pop(id)
    @statusbar.push(id, str)
    if block_given?
      yield
      @statusbar.pop(id)
    end
  end

  def clearStatusLine()
    id = @statusbar.get_context_id("statusbar")
    @statusbar.pop(id)
  end

  ## GTK callbacks: bind into presenter actions

  def gtk_main_quit
    Gtk.main_quit()
  end

  def menuopen_activate_cb(x)
    @presenter.loadSettingsAction
  end

  def menusave_activate_cb(x)
    @presenter.saveSettingsAction
  end

  def refresh_clicked_cb(x)
    @presenter.refreshAction
  end

  def devices_changed_cb(x)
    index = @devCombo.active
    if index >= 0
      @presenter.selectDeviceAction(index)
    end
  end

  def defaults_clicked_cb(x)
    @presenter.resetDefaultsAction
  end

  def remap_clicked_cb(x)
    @presenter.remapAction
  end

  def download_clicked_cb(x)
    @presenter.downloadSettingsAction
  end

  def upload_clicked_cb(x)
    @presenter.uploadSettingsAction
  end

  def program_clicked_cb(x)
    i = @programButtons.index(x)
    @presenter.handleProgramClick(i)
  end

  def keyboardDrawing_click_cb(area, event)
    x = event.x
    y = event.y

    if(@layout != nil)
      @layout.each.with_index do | lkey, i |
        kx, ky, kw, kh = key_dimensions(lkey)
        kx = kx + @pictureOriginX
        ky = ky + @pictureOriginY
        if x >= kx && x < (kx + kw) && y >= ky && y < (ky + kh)
          # found
          lkey_click_cb(keypad_adjust_index(i))
          break
        end
      end
    end
  end

  def lkey_click_cb(lkeyid)
    # if it's the keypad key, we switch our keypad mode
    if @keypad != nil && lkeyid == @keypad.key
      @keypadMode = !@keypadMode
      @keyboardDrawing.queue_draw
    else
      # otherwise pass on to the presenter
      @presenter.handleKeyclick(lkeyid)
    end
  end

  def keyboardDrawing_realize_cb(area)
  end

  def key_dimensions(lkey_entry)
    kx = lkey_entry["x"].to_i
    ky = lkey_entry["y"].to_i
    kw = lkey_entry["w"].to_i
    kh = lkey_entry["h"].to_i
    return kx, ky, kw, kh
  end

  def keypad_adjust_index(i)
    if @keypad != nil
      return @keypad.adjust_index(i, @keypadMode)
    else
      return i
    end
  end

  def keyboardDrawing_expose_cb(area, event)
    return if @picture == nil

    cr = area.window.create_cairo_context

    cr.set_source_rgb 0.1, 0.1, 0.1

    # draw keyboard picture centered. Save the origin so that
    # we can access it from the click handler
    alloc = area.allocation
    @pictureOriginX = alloc.width  / 2 - (@picture.width  / 2)
    @pictureOriginY = alloc.height / 2 - (@picture.height / 2)

    area.window.draw_pixbuf(area.style.fg_gc(area.state), @picture, 0, 0, @pictureOriginX, @pictureOriginY, -1, -1, Gdk::RGB::DITHER_NORMAL, 0, 0)

    if @mapping != nil
      # draw key labels if we have them
      @layout.each.with_index do | lkey, i |
        mapping = @mapping[keypad_adjust_index(i)]

        # get dimensions
        kx, ky, kw, kh = key_dimensions(lkey)

        #If we're in keypard mode, provide visual indication by highlighting:
        if @keypadMode
          cr.set_source_rgba 0, 0, 1, 0.2
          cr.rectangle @pictureOriginX+kx, @pictureOriginY+ky, kw, kh
          cr.fill
          cr.set_source_rgb 0.1, 0.1, 0.1
        end

        # look up name for mapped string
        name = HID_NAMES[mapping].split("_")

        line_sep = 3
        # Choose appropriate font size and calculate width/total height
        fs = 16
        begin
          cr.select_font_face("Helvetica",
                              Cairo::FONT_SLANT_NORMAL,
                              (fs > 14 ? Cairo::FONT_WEIGHT_BOLD : Cairo::FONT_WEIGHT_NORMAL))
          cr.set_font_size fs
          extents = name.map { |s| cr.text_extents(s) }
          maxWidth  = extents.map(&:width).max
          totHeight = extents.map(&:height).inject(&:+) + line_sep * (name.length - 1)
          fs -= 0.5
        end while(maxWidth > kw || totHeight > kh)

        # find location for key
        ty = @pictureOriginY + ky + (kh / 2) - (totHeight / 2)
        name.each do |seg|
          extents = cr.text_extents(seg)
          tx = @pictureOriginX + kx + (kw / 2) - (extents.width / 2)
          ty += extents.height # text is anchored bottom left, so add height before drawing
          cr.move_to(tx, ty)
          cr.show_text seg
          ty += line_sep
        end
      end
    end
  end
end
