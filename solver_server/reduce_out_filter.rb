# Filters output from Reduce (STDIN is Reduce output).
#
# 1. Removes the prompt and welcome message.
# 2. Makes responses single-line.
# 3. Remove terminating $
#

gets # Drop welcome message.

def get_more
  STDIN.readpartial(1_000_000)
rescue EOFError
  nil
end

response_buf = ""

while chunk = get_more
  response_buf << chunk
  if response_buf =~ /\n\d+: \z/ # Response over, or very first prompt.
    to_print = response_buf.gsub(/\d+: \z/, "").gsub(/\s+/, " ").strip.chomp("$")
    if to_print != ""
      puts to_print
      STDOUT.flush
    end
    response_buf = ""
  end
end
