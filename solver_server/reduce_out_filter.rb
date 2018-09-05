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
  if response_buf =~ /\n\d+: \z/ # Response(s) over, or very first prompt. (Responses can pile up if we send lots of queries really fast.)
    to_print =
      response_buf.split(/\n\d+: /).map do |response|
        response.gsub(/\s+/, " ").strip.chomp("$")
      end.join("\n").strip

    if to_print != ""
      puts to_print
      STDOUT.flush
    end
    response_buf = ""
  end
end
