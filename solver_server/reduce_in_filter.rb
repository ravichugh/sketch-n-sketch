# Filters input to reduce.
#
# Sets the formatting to draw fractions with a / instead of pretty printing.
#

puts("off ratpri;")

while raw_line = gets
  if raw_line !~ /;\s*\z/
    line = raw_line.sub(/\s*\z/, ";")
  else
    line = raw_line.chomp
  end
  puts line
  STDOUT.flush
end
