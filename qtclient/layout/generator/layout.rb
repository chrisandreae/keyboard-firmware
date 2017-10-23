#!/usr/bin/env ruby
# -*- indent-tabs-mode: nil -*-
require 'builder'
require 'cairo'

class Layout
  DEFAULT_KEY_SIZE = 50
  DEFAULT_PADDING  = 4

  Rect = Struct.new(:x, :y, :w, :h)
  Key  = Struct.new(:id, :rect)

  attr_reader :id, :name, :keys

  def initialize(id, name, key_size: DEFAULT_KEY_SIZE, padding: DEFAULT_PADDING, &block)
    @id   = id
    @name = name
    @padding = padding
    @keys = MatrixBuilder.new(key_size, padding)._evaluate(&block)
  end

  def image_name
    "#{id}.png"
  end

  def layout_name
    "#{id}.xml"
  end

  def dump
    File.open(layout_name, "w") { |f| f.write(to_xml) }
    to_image.write_to_png(image_name)
  end

  def to_xml(builder = Builder::XmlMarkup.new)
    builder.keyboard(layout: name, layoutid: id, image: image_name) do |b|
      b.layout do
        keys.each do |key|
          b.key(name: key.id,
                w: key.rect.w, h: key.rect.h,
                x: key.rect.x, y: key.rect.y)
        end
      end
    end
  end

  def to_image
    max_x, max_y = keys.inject([0,0]) do |(mx, my), key|
      [[mx, key.rect.x + key.rect.w].max,
       [my, key.rect.y + key.rect.h].max]
    end

    surface = Cairo::ImageSurface.new(max_x + @padding, max_y + @padding)
    context = Cairo::Context.new(surface)

    context.set_source_color(:white)
    context.paint

    context.line_width = 2
    context.set_source_color(:black)

    keys.each do |key|
      context.rectangle(key.rect.x, key.rect.y, key.rect.w, key.rect.h)
      context.stroke
    end

    surface
  end

  class MatrixBuilder
    def initialize(key_size, padding)
      @key_size = key_size
      @padding  = padding
      @keys     = []
      @next_id  = 0
    end

    def _evaluate(&block)
      self.instance_eval(&block)
      @keys.tap(&:freeze)
    end

    def _next_id
      @next_id.tap { @next_id += 1 }
    end

    def matrix_key(row, col)
      @keys << Key.new(_next_id,
                       Rect.new((@key_size + @padding) * col + @padding,
                                (@key_size + @padding) * row + @padding,
                                @key_size,
                                @key_size))
    end

    def square_matrix(rows, cols,
                      row_start: 0, row_stride: 1,
                      col_start: 0, col_stride: 1,
                      order: :rows)
      row = row_start
      col = col_start

      rows_left = rows
      cols_left = cols

      while rows_left > 0 && cols_left > 0
        matrix_key(row, col)

        case order
        when :rows
          col       += col_stride
          cols_left -= 1
          if cols_left == 0
            col       = col_start
            cols_left = cols
            row       += row_stride
            rows_left -= 1
          end

        when :cols
          row       += row_stride
          rows_left -= 1
          if rows_left == 0
            row       = row_start
            rows_left = rows
            col       += col_stride
            cols_left -= 1
          end
        end
      end
    end
  end
end
