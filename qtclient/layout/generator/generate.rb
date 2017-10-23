require_relative 'layout'

UGBOARD_LAYOUT = Layout.new(3, "ugboard") do
  # Right thumb
  square_matrix(2, 3, col_start: 12, row_start: 6, row_stride: +1, col_stride: -1, order: :rows)
  # Right keywell
  square_matrix(6, 7, col_start: 12, order: :cols)
  # Left thumb
  square_matrix(2, 3, col_start: 6, row_start: 6, row_stride: +1, col_stride: +1, order: :rows)
  # Left keywell
  square_matrix(6, 7, col_start: 6, col_stride: -1, order: :cols)
end

UGBOARD_LAYOUT.dump
