require "json"

class Event
  attr_reader :time, :name, :info

  def initialize(line)
    _, ms_since_epoch, @name, @info = line.split("\t", 4)

    @time = Time.at(ms_since_epoch.to_f / 1000.0)
  end

  def shift_down?
    name == "Key Down 16"
  end

  def escape_down?
    name == "Key Down 27"
  end

  def undo?
    name == "Undo"
  end

  def redo?
    name == "Redo"
  end

  def choose_deuce_exp?
    name =~ /^Choose Deuce Exp /
  end

  def json_info(key)
    JSON.parse(info)[key]
  rescue JSON::ParserError
    nil
  end

  def deuce_selections_count
    json_info("deuceSelectionsCount") && json_info("deuceSelectionsCount").to_i
  end

  # User Study Next   Step (HeadToHeadTask,("Three Rectangles",TextSelectOnly))

  def user_study_next?
    name == "User Study Next"
  end

  def next_step_head_to_head?
    user_study_next? && info =~ /^Step \(HeadToHeadTask/
  end

  def next_step_task_name
    info[/^Step \(\w+,\("([^"]+)"\,\w+\)\)/, 1]
  end

  def next_step_task_treatment
    info[/^Step \(\w+,\("[^"]+"\,(\w+)\)\)/, 1]
  end

  def deuce_exp_chosen
    name[/^Choose Deuce Exp "([\S\s]+)"/, 1]
  end

  def to_tsv
    [time, name, info, deuce_selections_count.to_i].join("\t")
  end
end

ROOT     = File.dirname(__FILE__)
LOGS_DIR = ROOT + "/user_study_logs"

participant_indices = Dir.entries(LOGS_DIR).join("\n").scan(/participant_(\d+)\.log/).flatten.map(&:to_i).sort

def split_into_head_to_head_tasks(events)
  events.
    slice_before(&:next_step_head_to_head?).
    select { |chunk| chunk[0].next_step_head_to_head? }.
    map { |chunk| [chunk[0]] + chunk[1..-1].slice_after(&:user_study_next?).to_a[0] }
end

def split_into_deuce_interactions(events)
  interactions = []
  current_interaction = nil
  pending_interaction = nil
  events.each do |event|
    if current_interaction
      if event.deuce_selections_count && event.deuce_selections_count == 0
        interactions << current_interaction
        current_interaction = nil
      else
        current_interaction << event
      end
    elsif pending_interaction
      if event.shift_down?
        pending_interaction = [event]
      elsif event.deuce_selections_count && event.deuce_selections_count > 0
        current_interaction = pending_interaction + [event]
        pending_interaction = nil
      else
        pending_interaction << event
      end
    else
      if event.shift_down?
        pending_interaction = [event]
      end
    end
  end

  interactions + (current_interaction || [])
end

puts [
  "Participant No",
  "Task",
  "Treatment",
  "Gross Time",
  "Interaction Time",
  "#Interactions",
  "Interacting Time",
  "#Undo",
  "#Redo",
  "Invocations",
].join("\t")

participant_indices.each do |participant_i|
  events = File.read("#{LOGS_DIR}/participant_#{participant_i}.log").split("\n").map { |line| Event.new(line) }

  head_to_head_tasks = split_into_head_to_head_tasks(events)

  head_to_head_tasks.each do |events|
    task_name = events[0].next_step_task_name
    treatment = events[0].next_step_task_treatment
    next if treatment == "ReadOnly"
    gross_start_time = events[0].time
    gross_end_time   = events[-1].time

    if treatment == "BoxSelectOnly"
      # events.each { |e| puts e.to_tsv }
      deuce_interactions = split_into_deuce_interactions(events)
      interactions_count = deuce_interactions.count
      if deuce_interactions.size == 0
        interaction_start_time = 0
        interaction_end_time = 0
      else
        interaction_start_time = deuce_interactions.first.first.time
        interaction_end_time   = deuce_interactions.last.last.time
      end
      interacting_time = deuce_interactions.map { |events| events.last.time - events.first.time }.sum
      invocations =
        events.
          flat_map do |event|
            if event.undo?
              ["Undo"]
            elsif event.redo?
              ["Redo"]
            elsif event.choose_deuce_exp?
              [event.deuce_exp_chosen]
            else
              []
            end
          end
    else
      interactions_count = -1
      interaction_start_time = 0
      interaction_end_time = 0
      interacting_time = 0
      invocations = []
    end

    puts [
      participant_i,
      task_name,
      treatment,
      gross_end_time - gross_start_time,
      interaction_end_time - interaction_start_time,
      interactions_count,
      interacting_time,
      events.count(&:undo?),
      events.count(&:redo?),
      invocations.join(";"),
    ].join("\t")

    # break
  end

end