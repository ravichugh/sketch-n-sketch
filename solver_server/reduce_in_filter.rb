# Filters input to reduce.
#

puts("off nat;")        # Turn off pretty printing to facilitate parsing.
puts("on combineexpt;") # More simplification of exponential terms multiplied by each other (uncommon).

while raw_line = gets
  if raw_line !~ /;\s*\z/
    line = raw_line.sub(/\s*\z/, ";")
  else
    line = raw_line.chomp
  end
  puts line
  STDOUT.flush
end
