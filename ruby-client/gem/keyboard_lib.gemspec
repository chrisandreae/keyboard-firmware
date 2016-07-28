# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'keyboard_lib/version'

Gem::Specification.new do |spec|
  spec.name          = "keyboard_lib"
  spec.version       = KeyboardLib::VERSION
  spec.authors       = ["Chris Andreae"]
  spec.email         = ["chris@andreae.gen.nz"]

  spec.summary       = %q{Ruby library for keyboard reprogramming}
  spec.homepage      = "https://github.com/chrisandreae/keyboard-firmware"

  spec.files         = `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }
  spec.bindir        = "exe"
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_dependency "libusb"

  spec.add_development_dependency "bundler", "~> 1.10"
  spec.add_development_dependency "rake", "~> 10.0"
end
