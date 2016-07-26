#!/usr/bin/env ruby
# -*- indent-tabs-mode: nil -*-

Hitbox = Struct.new(:x, :y, :w, :h)
Key = Struct.new(:hitbox) do
  def initialize(args)
    super
    @id = "k#{self.class.next_id}"
  end

  def image
  end

  def layout_entry
    %Q{<key name="#{@id}" w="#{hitbox.w}" h="#{hitbox.h}" x="#{hitbox.x}" y="#{hitbox.y}"/>}
  end

  @gensym_id = 0
  def self.next_id
    @gensym_id += 1
  end
end

class KeyBucket
  def initialize
    @keys = []
  end

  def <<(key)
    @keys << key
  end

  def layout
    ['<keyboard layout="ugboard" layoutid="3" image="ugboard.png"><layout>'] +
      @keys.map(&:layout_entry) +
      ['</layout></keyboard>']
  end
end

class Layout
  def layout_file
    bucket.layout
  end

  private def bucket
    @bucket ||= KeyBucket.new.tap { |b| build(b) }
  end

  class MatrixBuilder
    def square_matrix(rows, cols,
                      row_start: 0, row_stride: 1,
                      col_start: 0, col_stride: 1,
                      order: :rows)

      row = row_start
      col = col_start

      rows_left = rows
      cols_left = cols

      while rows_left > 0 && cols_left > 0
        matrix_key!(row, col)

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

class UgBoardLayout < Layout
  class UgBuilder < MatrixBuilder
    KEY_SIZE = 50

    def initialize(bucket)
      @bucket = bucket
    end

    def build!
      right_thumb
      right_keywell
      left_thumb
      left_keywell
    end

    def matrix_key!(row, col)
      @bucket << Key.new(Hitbox.new((KEY_SIZE) * col, (KEY_SIZE) * row, KEY_SIZE, KEY_SIZE))
    end

    private

    def left_keywell
      square_matrix(6, 7, col_start: 6, col_stride: -1, order: :cols)
    end

    def left_thumb
      square_matrix(2, 3, col_start: 6, row_start: 6, row_stride: +1, col_stride: +1, order: :rows)
    end

    def right_thumb
      square_matrix(2, 3, col_start: 12, row_start: 6, row_stride: +1, col_stride: -1, order: :rows)
    end

    def right_keywell
      square_matrix(6, 7, col_start: 12, order: :cols)
    end
  end

  private def build(b)
    UgBuilder.new(b).build!
  end
end

layout = UgBoardLayout.new
File.open('3.xml', 'w') { |f| f.puts(layout.layout_file) }
