#!/usr/local/bin/ruby

require_relative "keyboardcomm"
require_relative "hidcodes"
require_relative "keyboardmodel"
require_relative "keyboardpresenter"
require_relative "keyboardview"

KeyboardPresenter.new.showAction
