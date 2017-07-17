begin
  require "sinatra"
  require "webrick"
rescue LoadError
  system "gem install sinatra --no-ri --no-rdoc"

  Gem.clear_paths
  require "sinatra"
  require "webrick"
end

begin
  require "pry" # Install gem, and insert binding.pry to drop into a debugging REPL.
rescue LoadError
end

ROOT          = File.dirname(__FILE__)
LOGS_DIR      = ROOT + "/user_study_logs"
PUBLIC_FOLDER = ROOT + "/build/out"

enable :sessions

puts "Visit http://localhost:4567/ to participate in the user study."

set :public_folder, PUBLIC_FOLDER

system("mkdir '#{LOGS_DIR}'")

def get_participant_number
  if session[:participant_number].to_i > 0
    session[:participant_number].to_i
  else
    last_participant_n = Dir.entries(LOGS_DIR).join("\n").scan(/participant_(\d+)\.log/).flatten.map(&:to_i).max
    session[:participant_number] = (last_participant_n || 0) + 1
  end
end

get "/" do
  get_participant_number # Set participant number on first request.
  send_file(PUBLIC_FOLDER + "/index.html")
end

post "/log_event" do
  open("#{LOGS_DIR}/participant_#{get_participant_number}.log", "a") do |log|
    log.puts request.body.read
  end
end
