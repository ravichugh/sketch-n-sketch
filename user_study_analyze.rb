require "irb"
require "json"
require "csv"

ROOT             = File.dirname(__FILE__)
LOGS_DIR         = ROOT + "/user_study_logs"
SURVEYS_DIR      = ROOT + "/user_study_surveys"
MOUSE_USAGE_FILE = ROOT + "/participant_mouse_usage.txt"
TASKS_CSV        = ROOT + "/tasks.csv"
METRICS_CSV      = ROOT + "/metrics.csv"

CSV_FILE = open(TASKS_CSV, "w")
METRICS_FILE = open(METRICS_CSV, "w")

participant_indices = Dir.entries(LOGS_DIR).join("\n").scan(/participant_(\d+)\.log/).flatten.map(&:to_i).sort

class Array
  def mean
    sum / size.to_f
  end

  def variance
    mean = self.mean
    map { |x| (x - mean)**2.0 }.sum / (size - 1.0)
  end

  def std_dev
    Math.sqrt(variance)
  end

  def std_error
    std_dev / Math.sqrt(size)
  end

  def twice_std_error
    std_error * 2.0
  end

  def rough_confidence_interval
    mean    = self.mean
    std_dev = self.std_dev
    (mean-twice_std_error..mean+twice_std_error)
  end

  def bernoulli_mean
    map do |x|
      if x == true || x.to_s.downcase == "yes"
        1.0
      elsif x == false || x.to_s.downcase == "no"
        0.0
      end
    end.mean
  end

  # https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
  def bernoulli_rough_confidence_interval
    p = bernoulli_mean
    se = Math.sqrt( p*(1.0-p) / size )
    (p-2*se..p+2*se)
  end
end

class Event
  attr_reader :event_number, :time, :name, :info

  def initialize(line)
    event_number_str, _, ms_since_epoch, @name, @info = line.split("\t", 5)

    if event_number_str =~ /T/
      _, ms_since_epoch, @name, @info = line.split("\t", 4)

      event_number_str = ms_since_epoch
    end

    @event_number = event_number_str.to_i
    @time = Time.at(ms_since_epoch.to_f / 1000.0)
  end

  # This handles restarts approprately
  def sort_key
    [@time, @event_number]
  end

  def shift_down?
    name == "Key Down 16"
  end

  def shift_up?
    name == "Key Up 16"
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

  def clear_drag?
    name == "Clear Drag"
  end

  def close_dialog_box?
    name =~ /^Close Dialog Box /
  end

  def dialog_box_closed
    name[/^Close Dialog Box "([\S\s]*)"/, 1]
  end

  # Cancel Give Up
  def cancel_give_up?
    name == "Cancel Give Up"
  end

  def deuce_right_click?
    name == "Deuce Right Click"
  end

  # Drag Deuce Popup Panel
  # Drag Edit Code Popup Panel
  def drag_popup_panel?
    name =~ /^Drag .* Popup Panel/
  end

  def hide_menu?
    name == "Hide Menu"
  end

  # Hover Button "Add Argument..."
  # Hover Button "Copy Expression..."
  # Hover Button "Create Function..."
  # Hover Button "Inline Definition..."
  # Hover Button "Introduce Single Variable..."
  # Hover Button "Introduce Variable..."
  # Hover Button "Make Multi-line..."
  # Hover Button "Make Single Line..."
  # Hover Button "Merge Expressions into Function..."
  # Hover Button "Move Definition..."
  # Hover Button "Next Step ▸"
  # Hover Button "Remove Argument..."
  # Hover Button "Rename 'brian'..."
  # Hover Button "Rename 'endIndex'..."
  # Hover Button "Rename 'ravi'..."
  # Hover Button "Rename 'redRect'..."
  # Hover Button "Reorder Expressions..."
  # Hover Button "Swap Expressions..."
  # Hover Button "Syntax"
  # Hover Button "Text Select Mode"
  # Hover Button "USER STUDY"
  def hover_button?
    name =~ /^Hover Button /
  end

  def button_hovered
    name[/^Hover Button "([\S\s]*)"/, 1]
  end

  # Hover Deuce Result "Abstract fill over its constants" [1]
  # Hover Deuce Result "Abstract juiceRectangle over its constants" [1]
  # Hover Deuce Result "Abstract rect1 over its constants" [1]
  # Hover Deuce Result "Abstract rect1 over its named constants" [1]
  # Hover Deuce Result "Abstract rect2 over its constants" [1]
  # Hover Deuce Result "Abstract rect2 over its named constants" [1]
  # Hover Deuce Result "Abstract rect3 over its constants" [1]
  # Hover Deuce Result "Abstract rect3 over its named constants" [1]
  # Hover Deuce Result "Abstract redRect over its constants" [1]
  # Hover Deuce Result "Abstract redRect over its named constants" [1]
  # Hover Deuce Result "Abstract w over its constants" [1]
  # Hover Deuce Result "Add Arguments" [1]
  # Hover Deuce Result "Inline fill" [1]
  # Hover Deuce Result "Inline height" [1]
  # Hover Deuce Result "Inline rect1" [2]
  # Hover Deuce Result "Inline ring and target" [1]
  # Hover Deuce Result "Inline ring" [1]
  # Hover Deuce Result "Inline width" [1]
  # Hover Deuce Result "Inline x" [1]
  # Hover Deuce Result "Inline x, y, height, width, and fill" [1]
  # Hover Deuce Result "Inline y" [1]
  # Hover Deuce Result "Inline y" [2]
  # Hover Deuce Result "Insert Argument cx" [1]
  # Hover Deuce Result "Insert Argument cy" [1]
  # Hover Deuce Result "Insert Argument num" [1]
  # Hover Deuce Result "Introduce Variable" [1]
  # Hover Deuce Result "Introduce Variables" [1]
  # Hover Deuce Result "Make Single Line" [1]
  # Hover Deuce Result "Move ring" [1]
  # Hover Deuce Result "New variable: num" [1]
  # Hover Deuce Result "New variable: x" [1]
  # Hover Deuce Result "New variable: y" [1]
  # Hover Deuce Result "New variable: y_x" [1]
  # Hover Deuce Result "Remove Argument fill" [1]
  # Hover Deuce Result "Remove Argument h" [1]
  # Hover Deuce Result "Remove Argument startIndex" [1]
  # Hover Deuce Result "Remove Argument w" [1]
  # Hover Deuce Result "Rename 'blase' to..." [1]
  # Hover Deuce Result "Rename 'brian' to..." [1]
  # Hover Deuce Result "Rename 'endIndex' to..." [1]
  # Hover Deuce Result "Rename 'potato' to..." [1]
  # Hover Deuce Result "Rename 'rect1' to..." [1]
  # Hover Deuce Result "Rename 'redRect' to 'blase'" [1]
  # Hover Deuce Result "Rename 'redRect' to 'brian'" [1]
  # Hover Deuce Result "Rename 'redRect' to..." [1]
  # Hover Deuce Result "Rename 'startIndex' to..." [1]
  # Hover Deuce Result "Rename 'stroke' to..." [1]
  # Hover Deuce Result "Rename 'y' to..." [1]
  # Hover Deuce Result "Swap width and height" [1]
  # Hover Deuce Result "Swap width and height" [3]
  def hover_deuce_result?
    name =~ /^Hover Deuce Result /
  end

  def deuce_result_hovered
    name[/^Hover Deuce Result "([\S\s]*)"/, 1]
  end

  # The menu is now called "Code Tools"
  #
  # Hover Edit Code Top Menu Item "Add Argument..."
  # Hover Edit Code Top Menu Item "Align Expressions..."
  # Hover Edit Code Top Menu Item "Copy Expression..."
  # Hover Edit Code Top Menu Item "Create Function..."
  # Hover Edit Code Top Menu Item "Duplicate Definition..."
  # Hover Edit Code Top Menu Item "Flip Boolean..."
  # Hover Edit Code Top Menu Item "Inline Definition..."
  # Hover Edit Code Top Menu Item "Introduce Single Variable..."
  # Hover Edit Code Top Menu Item "Introduce Variable..."
  # Hover Edit Code Top Menu Item "Make Multi-line..."
  # Hover Edit Code Top Menu Item "Make Single Line..."
  # Hover Edit Code Top Menu Item "Merge Expressions into Function..."
  # Hover Edit Code Top Menu Item "Move Definition..."
  # Hover Edit Code Top Menu Item "Remove Argument..."
  # Hover Edit Code Top Menu Item "Rename Variable..."
  # Hover Edit Code Top Menu Item "Reorder Arguments..."
  # Hover Edit Code Top Menu Item "Reorder Expressions..."
  # Hover Edit Code Top Menu Item "Swap Expressions..."
  # Hover Edit Code Top Menu Item "Swap Names and Usages..."
  # Hover Edit Code Top Menu Item "Swap Usages..."
  def hover_edit_code_top_menu_item?
    name =~ /^Hover Edit Code Top Menu Item /
  end

  def edit_code_top_menu_item_hovered
    name[/^Hover Edit Code Top Menu Item "([\S\s]*)"/, 1]
  end

  # Hover Menu "Code Tools"
  # Hover Menu "Help"
  # Hover Menu "Options"
  # Hover Menu "View"
  def hover_menu?
    name =~ /^Hover Menu /
  end

  def menu_hovered
    name[/^Hover Menu "([\S\s]*)"/, 1]
  end

  # Hover Redo
  def hover_redo?
    name == "Hover Redo"
  end

  # Hover Undo
  def hover_undo?
    name == "Hover Undo"
  end

  # Leave Button "Add Argument..."
  # Leave Button "Copy Expression..."
  # Leave Button "Create Function..."
  # Leave Button "Inline Definition..."
  # Leave Button "Introduce Single Variable..."
  # Leave Button "Introduce Variable..."
  # Leave Button "Make Multi-line..."
  # Leave Button "Make Single Line..."
  # Leave Button "Merge Expressions into Function..."
  # Leave Button "Move Definition..."
  # Leave Button "Next Step ▸"
  # Leave Button "Remove Argument..."
  # Leave Button "Rename 'brian'..."
  # Leave Button "Rename 'endIndex'..."
  # Leave Button "Rename 'ravi'..."
  # Leave Button "Rename 'redRect'..."
  # Leave Button "Reorder Expressions..."
  # Leave Button "Swap Expressions..."
  # Leave Button "Syntax"
  # Leave Button "Text Select Mode"
  # Leave Button "USER STUDY"
  def leave_button?
    name =~ /^Leave Button /
  end

  def button_left
    name[/^Leave Button "([\S\s]*)"/, 1]
  end

  # Leave Deuce Result "Abstract rect1 over its constants" [1]
  # Leave Deuce Result "Abstract rect1 over its named constants" [1]
  # Leave Deuce Result "Abstract rect2 over its constants" [1]
  # Leave Deuce Result "Abstract rect3 over its constants" [1]
  # Leave Deuce Result "Abstract rect3 over its named constants" [1]
  # Leave Deuce Result "Abstract redRect over its constants" [1]
  # Leave Deuce Result "Abstract redRect over its named constants" [1]
  # Leave Deuce Result "Inline rect1" [2]
  # Leave Deuce Result "Inline ring and target" [1]
  # Leave Deuce Result "Inline y" [2]
  # Leave Deuce Result "Introduce Variable" [1]
  # Leave Deuce Result "Introduce Variables" [1]
  # Leave Deuce Result "Make Single Line" [1]
  # Leave Deuce Result "New variable: num" [1]
  # Leave Deuce Result "New variable: x" [1]
  # Leave Deuce Result "New variable: y" [1]
  # Leave Deuce Result "New variable: y_x" [1]
  # Leave Deuce Result "Remove Argument fill" [1]
  # Leave Deuce Result "Rename 'brian' to 'BRIAN'" [1]
  # Leave Deuce Result "Rename 'brian' to..." [1]
  # Leave Deuce Result "Rename 'redRect' to 'blase'" [1]
  # Leave Deuce Result "Rename 'redRect' to 'brian'" [1]
  # Leave Deuce Result "Rename 'redRect' to..." [1]
  # Leave Deuce Result "Rename 'startIndex' to..." [1]
  # Leave Deuce Result "Rename 'y' to..." [1]
  def leave_deuce_result?
    name =~ /^Leave Deuce Result /
  end

  def deuce_result_left
    name[/^Leave Deuce Result "([\S\s]*)"/, 1]
  end

  # Leave Edit Code Top Menu Item "Add Argument..."
  # Leave Edit Code Top Menu Item "Align Expressions..."
  # Leave Edit Code Top Menu Item "Copy Expression..."
  # Leave Edit Code Top Menu Item "Create Function..."
  # Leave Edit Code Top Menu Item "Duplicate Definition..."
  # Leave Edit Code Top Menu Item "Flip Boolean..."
  # Leave Edit Code Top Menu Item "Inline Definition..."
  # Leave Edit Code Top Menu Item "Introduce Single Variable..."
  # Leave Edit Code Top Menu Item "Introduce Variable..."
  # Leave Edit Code Top Menu Item "Make Multi-line..."
  # Leave Edit Code Top Menu Item "Make Single Line..."
  # Leave Edit Code Top Menu Item "Merge Expressions into Function..."
  # Leave Edit Code Top Menu Item "Move Definition..."
  # Leave Edit Code Top Menu Item "Remove Argument..."
  # Leave Edit Code Top Menu Item "Rename 'brian'..."
  # Leave Edit Code Top Menu Item "Rename 'redRect'..."
  # Leave Edit Code Top Menu Item "Rename Variable..."
  # Leave Edit Code Top Menu Item "Reorder Arguments..."
  # Leave Edit Code Top Menu Item "Reorder Expressions..."
  # Leave Edit Code Top Menu Item "Swap Expressions..."
  # Leave Edit Code Top Menu Item "Swap Names and Usages..."
  # Leave Edit Code Top Menu Item "Swap Usages..."
  def leave_edit_code_top_menu_item?
    name =~ /^Leave Edit Code Top Menu Item /
  end

  def edit_code_top_menu_item_left
    name[/^Leave Edit Code Top Menu Item "([\S\s]*)"/, 1]
  end

  # Leave Menu "Code Tools"
  # Leave Menu "Help"
  # Leave Menu "Options"
  # Leave Menu "View"
  def leave_menu?
    name =~ /^Leave Menu /
  end

  def menu_left
    name[/^Leave Menu "([\S\s]*)"/, 1]
  end

  # Leave Redo
  def leave_redo?
    name == "Leave Redo"
  end

  # Leave Undo
  def leave_undo?
    name == "Leave Undo"
  end

  # Load Icon
  def load_icon?
    name == "Load Icon"
  end

  # MouseIsDown False
  def mouse_is_down_false?
    name == "MouseIsDown False"
  end

  # MouseIsDown True
  def mouse_is_down_true?
    name == "MouseIsDown True"
  end

  # New
  def new?
    name == "New"
  end

  # New Code
  def new_code?
    name == "New Code"
  end

  def new_code
    eval info
  end

  # New Deuce Tools and Results List
  def new_deuce_tools_and_results_list?
    name == "New Deuce Tools and Results List"
  end

  # TODO which are shown?
  def new_deuce_tools_and_results_list
    raw_json_info { [] }
  end

  # Noop
  def noop?
    name == "Noop"
  end

  # Open Dialog Box "Help HelpSyntax"
  # Open Dialog Box "Help HelpTextSelectMode"
  def open_dialog_box?
    name =~ /^Open Dialog Box /
  end

  def dialog_box_opened
    name[/^Open Dialog Box "([\S\s]*)"/, 1]
  end

  # Give Up Asker
  def give_up_asker?
    name == "Give Up Asker"
  end

  # Run
  def run?
    name == "Run"
  end

  # Run Error
  def run_error?
    name == "Run Error"
  end

  def run_error
    eval info
  end

  # Run Success
  def run_success?
    name == "Run Success"
  end

  # Set Selected Deuce Tool "Add Argument"
  # Set Selected Deuce Tool "Align Expressions"
  # Set Selected Deuce Tool "Create Function"
  # Set Selected Deuce Tool "Inline Definition"
  # Set Selected Deuce Tool "Introduce Single Variable"
  # Set Selected Deuce Tool "Introduce Variable"
  # Set Selected Deuce Tool "Make Single Line"
  # Set Selected Deuce Tool "Merge Expressions into Function"
  # Set Selected Deuce Tool "Move Definition"
  # Set Selected Deuce Tool "Remove Argument"
  # Set Selected Deuce Tool "Rename 'brian'"
  # Set Selected Deuce Tool "Rename 'endIndex'"
  # Set Selected Deuce Tool "Rename 'redRect'"
  # Set Selected Deuce Tool "Rename Variable"
  # Set Selected Deuce Tool "Swap Expressions"
  # Set Selected Deuce Tool "Swap Names and Usages"
  def set_selected_deuce_tool?
    name =~ /^Set Selected Deuce Tool /
  end

  def deuce_tool_selected
    name[/^Set Selected Deuce Tool "([\S\s]*)"/, 1]
  end

  # Showing Right Click Menu
  def showing_right_click_menu?
    name == "Showing Right Click Menu"
  end

  # Text Cursor Moved
  def text_cursor_moved?
    name == "Text Cursor Moved"
  end

  # Text Selection Changed
  def text_selection_changed?
    name == "Text Selection Changed"
  end

  def changed_text_selection
    raw_json_info { [] }
  end

  # Toggle Menu
  def toggle_menu?
    name == "Toggle Menu"
  end

  # Update Rename Var Text Box: B
  # Update Rename Var Text Box: BR
  # Update Rename Var Text Box: BRI
  # Update Rename Var Text Box: BRIA
  # Update Rename Var Text Box: BRIAN
  # Update Rename Var Text Box: b
  # Update Rename Var Text Box: bl
  # Update Rename Var Text Box: bla
  # Update Rename Var Text Box: blas
  # Update Rename Var Text Box: blase
  # Update Rename Var Text Box: br
  # Update Rename Var Text Box: bri
  # Update Rename Var Text Box: bria
  # Update Rename Var Text Box: brian
  # Update Rename Var Text Box: d
  # Update Rename Var Text Box: df
  # Update Rename Var Text Box: dfs
  # Update Rename Var Text Box: l
  # Update Rename Var Text Box: li
  # Update Rename Var Text Box: lin
  # Update Rename Var Text Box: line
  # Update Rename Var Text Box: lineC
  # Update Rename Var Text Box: lineCo
  # Update Rename Var Text Box: lineCol
  # Update Rename Var Text Box: lineColo
  # Update Rename Var Text Box: lineColor
  # Update Rename Var Text Box: n
  # Update Rename Var Text Box: nu
  # Update Rename Var Text Box: num
  # Update Rename Var Text Box: numR
  # Update Rename Var Text Box: numRi
  # Update Rename Var Text Box: numRin
  # Update Rename Var Text Box: numRing
  # Update Rename Var Text Box: numRings
  # Update Rename Var Text Box: p
  # Update Rename Var Text Box: po
  # Update Rename Var Text Box: pot
  # Update Rename Var Text Box: pota
  # Update Rename Var Text Box: potat
  # Update Rename Var Text Box: potato
  # Update Rename Var Text Box: r
  # Update Rename Var Text Box: ra
  # Update Rename Var Text Box: rav
  # Update Rename Var Text Box: ravi
  # Update Rename Var Text Box: re
  # Update Rename Var Text Box: rec
  # Update Rename Var Text Box: rect
  # Update Rename Var Text Box: rect_
  # Update Rename Var Text Box: rect_5
  # Update Rename Var Text Box: rect_50
  # Update Rename Var Text Box: rect_50_
  # Update Rename Var Text Box: rect_50_7
  # Update Rename Var Text Box: rect_50_70
  def update_rename_var_text_box?
    name =~ /^Update Rename Var Text Box: /
  end

  def rename_var_text_box_text
    name[/^Update Rename Var Text Box: ([\S\s]*)/, 1]
  end

  # User Has Typed
  def user_has_typed?
    name == "User Has Typed"
  end

  # Visibility Change
  def visibility_change?
    name == "Visibility Change"
  end

  # Window Dimensions
  def window_dimensions?
    name == "Window Dimensions"
  end

  # msgMouseClickDeuceWidget DeuceExp 6791
  # msgMouseClickDeuceWidget DeuceExpTarget (After,6795)
  # msgMouseClickDeuceWidget DeuceLetBindingEquation 6805
  # msgMouseClickDeuceWidget DeucePat ((6813,1),[1])
  # msgMouseClickDeuceWidget DeucePatTarget (After,((6796,1),[]))
  def mouse_click_deuce_widget?
    name =~ /^msgMouseClickDeuceWidget /
  end

  def deuce_widget_clicked
    name[/^msgMouseClickDeuceWidget ([\S\s]*)/, 1]
  end

  # msgMouseEnterDeuceWidget DeuceExp 6790
  # msgMouseEnterDeuceWidget DeuceExpTarget (After,6791)
  # msgMouseEnterDeuceWidget DeuceLetBindingEquation 6795
  # msgMouseEnterDeuceWidget DeucePat ((6795,1),[])
  # msgMouseEnterDeuceWidget DeucePatTarget (After,((6795,1),[]))
  def mouse_enter_deuce_widget?
    name =~ /^msgMouseEnterDeuceWidget /
  end

  def deuce_widget_entered
    name[/^msgMouseEnterDeuceWidget ([\S\s]*)/, 1]
  end

  # msgMouseLeaveDeuceWidget DeuceExp 6790
  # msgMouseLeaveDeuceWidget DeuceExpTarget (After,6791)
  # msgMouseLeaveDeuceWidget DeuceLetBindingEquation 6795
  # msgMouseLeaveDeuceWidget DeucePat ((6795,1),[])
  # msgMouseLeaveDeuceWidget DeucePatTarget (After,((6795,1),[]))
  def mouse_leave_deuce_widget?
    name =~ /^msgMouseLeaveDeuceWidget /
  end

  def deuce_widget_left
    name[/^msgMouseLeaveDeuceWidget ([\S\s]*)/, 1]
  end

  def raw_json_info
    JSON.parse(info.to_s)
  rescue JSON::ParserError
    yield # do default
  end

  def json_info(key)
    parsed = raw_json_info { {} }
    (parsed.is_a?(Hash) || nil) && parsed[key]
  end

  def deuce_selections_count
    json_info("deuceSelectionsCount") && Integer(json_info("deuceSelectionsCount"))
  end

  def code_annotations_count
    json_info("codeAnnotationsCount") && Integer(json_info("codeAnnotationsCount"))
  end

  def task_timeout?
    name == "Task Timeout"
  end

  def user_study_next?
    name =~ /^(New: )?User Study Next/
  end

  def next_step_task?
    user_study_next? && info =~ /^(Step )?\((Task )?(HeadToHead|OpenEnded)(Task)?/
  end

  def next_step_head_to_head?
    user_study_next? && info =~ /^(Step )?\((Task )?HeadToHead(Task)?/
  end

  def next_step_task_name
    info[/^(?:Step )?\([\w ]+,\("([^"]+)"\,\w+\)\)/, 1]
  end

  def next_step_task_treatment
    info[/^(?:Step )?\([\w ]+,\("[^"]+"\,(\w+)\)\)/, 1]
  end

  def deuce_exp_chosen
    name[/^Choose Deuce Exp "([\S\s]*)"/, 1]
  end

  def to_tsv
    [event_number, time, name, info, deuce_selections_count.to_i, code_annotations_count.to_i].join("\t")
  end
end

def split_into_tasks(events)
  events.
    slice_before(&:next_step_task?).
    select { |chunk| chunk[0].next_step_task? }.
    map { |chunk| [chunk[0]] + chunk[1..-1].slice_after(&:user_study_next?).to_a[0] }
end

class Interaction
  attr_reader :events

  def initialize(events)
    @events = events
  end

  def begin
    events.first.time
  end

  def end
    events.last.time
  end

  def duration
    time_range.duration
  end

  def time_range
    TimeRange.new(self.begin, self.end)
  end
end

class TimeRange < Range
  def initialize(begin_, end_)
    if end_ < begin_
      super(begin_, begin_, true)
    else
      super(begin_, end_, true)
    end
  end

  def duration
    self.end - self.begin
  end

  def empty?
    duration == 0
  end

  # Returns a TimeRanges, since other can take a slice out of the middle, or annihilate this range completely.
  def -(other)
    if other.begin <= self.begin
      ranges = [TimeRange.new([other.end, self.begin].max, self.end)]
    elsif other.end >= self.end
      ranges = [TimeRange.new(self.begin, [other.begin, self.end].min)]
    else
      ranges = [TimeRange.new(self.begin, other.begin), TimeRange.new(other.end, self.end)]
    end
    TimeRanges.new(ranges.reject(&:empty?))
  end
end

class TimeRanges < Array
  def -(other)
    case other
    when TimeRange
      TimeRanges.new(flat_map { |time_range| time_range - other })
    when TimeRanges
      new_ranges = self
      other.each do |other_range|
        new_ranges -= other_range
      end
      new_ranges
    end
  end

  # Assumes ranges are non-overlapping
  def duration
    map(&:duration).sum
  end
end

class BoxSelectInteraction < Interaction
end

class TextSelectInteraction < Interaction
end

class DialogInteraction < Interaction
end

# A text select interaction starts when the selection is started or right mouse is clicked or the code tools menu is opened
# and ends when the selections are cleared (via refactoring, or escape key, or deselection)
#
# Text selections that do not result in a right click or code tools menu views that do not result in selecting a refactoring
# to configure do not count as an interaction
def split_into_text_select_interactions(events)
  interactions = []
  current_interaction_events      = nil
  pending_interaction_events      = nil
  current_text_selection          = []
  last_top_menu_item_hovered      = nil
  top_menus_open                  = false
  pending_interaction_is_top_menu = false

  events.each do |event|
    if event.shift_down?
      shift_depressed = true
    elsif event.shift_up?
      shift_depressed = false
    end

    if event.text_selection_changed?
      current_text_selection = event.changed_text_selection
    end

    if event.hover_menu?
      last_top_menu_item_hovered = event.menu_hovered
    elsif event.toggle_menu?
      top_menus_open = !top_menus_open
    elsif event.hide_menu?
      top_menus_open = false
    end

    if current_interaction_events
      if event.escape_down? || event.choose_deuce_exp? || event.undo? || event.redo? # TODO check all places where this info is reset
        current_interaction_events << event
        interactions << TextSelectInteraction.new(current_interaction_events)
        current_interaction_events = nil
      end
    elsif pending_interaction_events
      pending_interaction_events << event
      if event.showing_right_click_menu?
        current_interaction_events = pending_interaction_events
        pending_interaction_events = nil
      elsif event.set_selected_deuce_tool?
        # Menu item selected
        current_interaction_events = pending_interaction_events
        pending_interaction_events = nil
      elsif event.text_selection_changed? && event.changed_text_selection.size == 0
        pending_interaction_events = nil
      elsif !top_menus_open && event.changed_text_selection.size == 0
        pending_interaction_events = nil
      end
    else
      if event.showing_right_click_menu?
        current_interaction_events = [event]
      elsif event.text_selection_changed? && event.changed_text_selection.size > 0
        pending_interaction_events = [event]
      elsif top_menus_open && last_top_menu_item_hovered == "Code Tools"
        pending_interaction_events = [event]
      end
    end
  end

  if current_interaction_events
    interactions + [TextSelectInteraction.new(current_interaction_events)]
  else
    interactions
  end
end

# A box select interaction is a series of deuce selections
# Starting when the shift key is depressed
# Ending when the selections are cleared (via refactoring, or escape key, or deselection) AND the shift key is also not depressed
#
# Taps of the shift key that do not result in a selection do not count as an interaction.
def split_into_box_select_interactions(events)
  interactions = []
  current_interaction_events = nil
  pending_interaction_events = nil
  shift_depressed     = false
  events.each do |event|
    if event.shift_down?
      shift_depressed = true
    elsif event.shift_up?
      shift_depressed = false
    end

    if current_interaction_events
      if event.escape_down? || event.choose_deuce_exp? || event.undo? || event.redo? # TODO check all places where this info is reset
        current_interaction_events << event
        interactions << BoxSelectInteraction.new(current_interaction_events)
        current_interaction_events = nil
      elsif event.deuce_selections_count && event.deuce_selections_count == 0 && !shift_depressed
        interactions << BoxSelectInteraction.new(current_interaction_events)
        current_interaction_events = nil
      else
        current_interaction_events << event
      end
    elsif pending_interaction_events
      if !shift_depressed && event.deuce_selections_count == 0
        pending_interaction_events = nil
      elsif event.deuce_selections_count && event.deuce_selections_count > 0
        current_interaction_events = pending_interaction_events + [event]
        pending_interaction_events = nil
      else
        pending_interaction_events << event
      end
    else
      if event.shift_down? && event.deuce_selections_count == 0 # Not typing in rename box
        pending_interaction_events = [event]
      end
    end
  end

  if current_interaction_events
    interactions + [BoxSelectInteraction.new(current_interaction_events)]
  else
    interactions
  end
end

def split_into_viewing_dialog_interactions(events)
  interactions = []
  current_interaction_events = nil
  events.each do |event|
    if current_interaction_events
      current_interaction_events << event
      if event.close_dialog_box? || event.cancel_give_up? || event.user_study_next? # next b/c task timeout
        interactions << DialogInteraction.new(current_interaction_events)
        current_interaction_events = nil
      elsif event.open_dialog_box? || event.give_up_asker?
        raise "nested dialog interactions!!"
      end
    elsif event.open_dialog_box? || event.give_up_asker?
      current_interaction_events = [event]
    elsif event.close_dialog_box? || event.cancel_give_up?
      raise "hanging close dialog event!!"
    end
  end

  if current_interaction_events
    interactions + [DialogInteraction.new(current_interaction_events)]
  else
    interactions
  end
end

# Extract time ranges out of event log.
#
# Except for viewing help dialogs and dragging the popup panel, ranges should be mutually exclusive.
#
# -
# - Drag Panel
# -

class Task
  attr_reader :participant_i, :events

  attr_accessor :number

  def initialize(participant_i, used_mouse, used_own_computer, events)
    @participant_i     = participant_i
    @used_mouse        = used_mouse
    @used_own_computer = used_own_computer
    @events            = events
  end

  def used_mouse?
    @used_mouse
  end

  def used_own_computer?
    @used_own_computer
  end

  def used_provided_computer?
    if used_own_computer?.nil?
      nil
    else
      !used_own_computer?
    end
  end

  def name
    events[0].next_step_task_name
  end

  def second_encounter?
    (5..8).cover? number
  end

  def treatment
    events[0].next_step_task_treatment
  end

  def events_intact?
    events_intact = true

    last_event_number = nil
    events.each do |event|
      if last_event_number && event.event_number != last_event_number + 1
        events_intact = false
        # puts "Participant #{participant_i}: event #{event.event_number} comes right after #{last_event_number}"
      end
      last_event_number = event.event_number
    end

    events_intact
  end

  def gross_start_time
    events[0].time
  end

  def gross_end_time
    events[-1].time
  end

  def gross_time
    gross_end_time - gross_start_time
  end

  # These next two should be mutually exclusive (TODO: check)

  def text_select_interactions
    @text_select_interactions ||= split_into_text_select_interactions(events)
  end

  def box_select_interactions
    @box_select_interactions ||= split_into_box_select_interactions(events)
  end

  def all_interactions
    @all_interactions ||= (text_select_interactions + box_select_interactions).sort_by(&:begin)
  end

  def interaction_start_time
    all_interactions.any? ? all_interactions.first.begin : 0
  end

  def interaction_end_time
    all_interactions.any? ? all_interactions.last.end : 0
  end

  def interaction_time
    interaction_end_time - interaction_start_time
  end

  def viewing_dialog_interactions
    @viewing_dialog_interactions ||= split_into_viewing_dialog_interactions(events)
  end

  def viewing_dialog_time_ranges
    TimeRanges.new(viewing_dialog_interactions.map(&:time_range))
  end

  def text_select_interacting_time
    (TimeRanges.new(text_select_interactions.map(&:time_range)) - viewing_dialog_time_ranges).duration
  end

  def box_select_interacting_time
    (TimeRanges.new(box_select_interactions.map(&:time_range)) - viewing_dialog_time_ranges).duration
  end

  def interacting_time
    text_select_interacting_time + box_select_interacting_time
  end

  def invocations
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
  end

  def completed?
    events.map(&:code_annotations_count).compact.last == 0
  end

  def timeout?
    events.any?(&:task_timeout?)
  end

  def refactorings_count
    events.count(&:choose_deuce_exp?)
  end

  def text_select_refactorings
    text_select_interactions.flat_map(&:events).select(&:choose_deuce_exp?)
  end

  def single_arg_text_select_refactorings
    text_select_refactorings.select { |e| e.deuce_selections_count == 1 }
  end

  def multi_arg_text_select_refactorings
    text_select_refactorings.select { |e| e.deuce_selections_count >= 2 }
  end

  def box_select_refactorings
    box_select_interactions.flat_map(&:events).select(&:choose_deuce_exp?)
  end

  def single_arg_box_select_refactorings
    box_select_refactorings.select { |e| e.deuce_selections_count == 1 }
  end

  def multi_arg_box_select_refactorings
    box_select_refactorings.select { |e| e.deuce_selections_count >= 2 }
  end

  def undo_count
    events.count(&:undo?)
  end

  def redo_count
    events.count(&:redo?)
  end

  def to_row
    [
      participant_i,
      used_mouse? ? "yes" : "no",
      used_own_computer? ? "yes" : "no",
      number,
      second_encounter? ? "yes" : "no",
      name,
      treatment,
      events_intact? ? "yes" : "no",
      completed? ? "yes" : "no",
      timeout? ? "yes" : "no",
      gross_time,
      interaction_time,
      text_select_interactions.count,
      text_select_interacting_time,
      text_select_refactorings.count,
      single_arg_text_select_refactorings.count,
      multi_arg_text_select_refactorings.count,
      box_select_interactions.count,
      box_select_interacting_time,
      box_select_refactorings.count,
      single_arg_box_select_refactorings.count,
      multi_arg_box_select_refactorings.count,
      all_interactions.count,
      interacting_time,
      invocations.size - invocations.grep(/^(Undo|Redo)$/).size,
      undo_count,
      redo_count,
      invocations.join("; "),
    ]
  end
end

# {
#   "experience": "3",
#   "functional_experience": "2",
#   "other_refactoring_tools": "yes",
#   "other_refactoring_tools_explanation": "Java and IntelliJ, once every few weeks for about 8 months.",
#   "prior_sns": "no",
#   "prior_sns_code_tools": "no",
#   "prior_sns_explanation": "",
#   "one_rectangle_mode_comparison": "2",
#   "two_circles_mode_comparison": "2",
#   "three_rectangles_mode_comparison": "2",
#   "target_icon_mode_comparison": "-1",
#   "mode_comparison_explanation": "Text select mode is easier when selecting individual variables, like when using the rename tool. Box select is better for selecting blocks of text and selecting many variables at once.",
#   "sus_1": "3",
#   "sus_2": "1",
#   "sus_3": "3",
#   "sus_4": "2",
#   "sus_5": "4",
#   "sus_6": "1",
#   "sus_7": "4",
#   "sus_8": "1",
#   "sus_9": "2",
#   "sus_10": "1",
#   "four_squares_explanation": "Not for selecting many variables at once (same issue as lambda icon task)",
#   "lambda_icon_explanation": "For the most part yes, but it was kind of annoying to select everything necessary for Make Equal By Copying. ",
#   "computer": "personal",
#   "improvements": "A feature that would automatically try to choose the best place (one that doesn't cover much code) for the toolbox to pop up upon right click/box select",
#   "comments": "",
#   "desired_other_domains": "",
#   "submission_time": "2017-08-15 09:31:49 -0500"
# }
class Survey
  attr_reader :json

  def initialize(json_str)
    @json = JSON.parse(json_str)
  end

  def years_experience
    Float(json["experience"])
  end

  def years_functional_experience
    Float(json["functional_experience"])
  end

  def other_refactoring_tools
    json["other_refactoring_tools"]
  end

  def prior_sns
    json["prior_sns"]
  end

  def prior_sns_code_tools
    json["prior_sns_code_tools"]
  end

  def one_rectangle_mode_comparison
    json["one_rectangle_mode_comparison"] && json["one_rectangle_mode_comparison"].to_i
  end

  def two_circles_mode_comparison
    json["two_circles_mode_comparison"] && json["two_circles_mode_comparison"].to_i
  end

  def three_rectangles_mode_comparison
    json["three_rectangles_mode_comparison"] && json["three_rectangles_mode_comparison"].to_i
  end

  def target_icon_mode_comparison
    json["target_icon_mode_comparison"] && json["target_icon_mode_comparison"].to_i
  end

  def sus_score
    sus_scores =
      (1..10).map do |i|
        begin
          i%2 == 1 ? Integer(json["sus_#{i}"]) - 1 : 5 - Integer(json["sus_#{i}"])
        rescue
          nil
        end
      end

    # If at least half of the SUS is completed, use it. Fill in blanks with middle value 3.
    # (Per Brooke: "If a respondent feels that they cannot respond to a particular item, they should mark the centre point of the scale.")
    if sus_scores.compact.size >= 5
      sus_scores.map! { |x| x.nil? ? 3-1 : x }
      sus_scores.sum * 2.5
    else
      nil
    end
  end

  def computer
    json["computer"]
  end
end

headers = [
  "participantNumber",
  "usedMouse",
  "usedOwnComputer",
  "taskNumber",
  "secondEncounter",
  "task",
  "treatment",
  "allEventsLogged",
  "completed",
  "timedOut",
  "grossTime",
  "interactionTime",
  "textSelectInteractionsStartedCount",
  "textSelectInteractingTime",
  "textSelectRefactoringsCount",
  "singleArgTextSelectRefactoringsCount",
  "multiArgTextSelectRefactoringsCount",
  "boxSelectInteractionsStartedCount",
  "boxSelectInteractingTime",
  "boxSelectRefactoringsCount",
  "singleArgBoxSelectRefactoringsCount",
  "multiArgBoxSelectRefactoringsCount",
  "interactionsStartedCount",
  "interactingTime",
  "refactoringsCount",
  "undoCount",
  "redoCount",
  "invocations",
]

puts headers.join("\t")
CSV_FILE.print(headers.to_csv)

all_surveys = []
all_tasks   = []
user_events = []
mouse_usage = File.read(MOUSE_USAGE_FILE).split.map { |word| word == "Mouse" }

participant_indices.each do |participant_i|
  next if participant_i >= 100

  survey = Survey.new(File.read("#{SURVEYS_DIR}/participant_#{participant_i}.json"))
  all_surveys << survey

  events = File.read("#{LOGS_DIR}/participant_#{participant_i}.log").split("\n").map { |line| Event.new(line) }.sort_by(&:sort_key) # Events are sent async and are occasionally slightly out of order

  user_events << events

  tasks = split_into_tasks(events)

  task_num = 0

  used_mouse = mouse_usage[participant_i-1]

  tasks.each do |events|
    task = Task.new(participant_i, used_mouse, survey.computer == "personal", events)
    # task_name = events[0].next_step_task_name
    # treatment = events[0].next_step_task_treatment

    next if task.treatment == "ReadOnly"

    # events_intact = true
    #
    # last_event_number = nil
    # events.each do |event|
    #   if last_event_number && event.event_number != last_event_number + 1
    #     events_intact = false
    #     # puts "Participant #{participant_i}: event #{event.event_number} comes right after #{last_event_number}"
    #   end
    #   last_event_number = event.event_number
    # end
    #
    # gross_start_time = events[0].time
    # gross_end_time   = events[-1].time
    # gross_time = gross_end_time - gross_start_time

    next if task.gross_time < 30.0 && task.events.none?(&:choose_deuce_exp?) # Skipping tasks because of crash

    task_num += 1

    task.number = task_num

    next if participant_i == 3 && task.name == "Target Icon" && task.treatment == "TextSelectOnly"  # Crash
    next if participant_i == 7 && task.name == "One Rectangle" && task.treatment == "BoxSelectOnly" # Crash

    all_tasks << task

    # # These should be mutually exclusive (TODO: check)
    # text_select_interactions = split_into_text_select_interactions(events)
    # box_select_interactions = split_into_box_select_interactions(events)
    # all_interactions = text_select_interactions + box_select_interactions
    # all_interactions.sort_by!(&:begin)
    # if all_interactions.size == 0
    #   interaction_start_time = 0
    #   interaction_end_time = 0
    # else
    #   interaction_start_time = all_interactions.first.begin
    #   interaction_end_time   = all_interactions.last.end
    # end
    # viewing_dialog_interactions = split_into_viewing_dialog_interactions(events)
    # text_select_interacting_time = (TimeRanges.new(text_select_interactions.map(&:time_range)) - TimeRanges.new(viewing_dialog_interactions.map(&:time_range))).duration
    # box_select_interacting_time  = (TimeRanges.new(box_select_interactions.map(&:time_range))  - TimeRanges.new(viewing_dialog_interactions.map(&:time_range))).duration
    # interacting_time = text_select_interacting_time + box_select_interacting_time
    # invocations =
    #   events.
    #     flat_map do |event|
    #       if event.undo?
    #         ["Undo"]
    #       elsif event.redo?
    #         ["Redo"]
    #       elsif event.choose_deuce_exp?
    #         [event.deuce_exp_chosen]
    #       else
    #         []
    #       end
    #     end

    # events.each do |event|
    #   if viewing_dialog_interactions.any? {|interaction| interaction.events.first == event}
    #     puts "*** VIEWING DIALOG INTERACTION BEGIN ***"
    #   end
    #   if text_select_interactions.any? {|interaction| interaction.events.first == event}
    #     puts "*** TEXT SELECT INTERACTION BEGIN ***"
    #   end
    #   if box_select_interactions.any? {|interaction| interaction.events.first == event}
    #     puts "*** BOX SELECT INTERACTION BEGIN ***"
    #   end
    #
    #   puts event.to_tsv
    #
    #   if box_select_interactions.any? {|interaction| interaction.events.last == event}
    #     puts "*** BOX SELECT INTERACTION END ***"
    #   end
    #   if text_select_interactions.any? {|interaction| interaction.events.last == event}
    #     puts "*** TEXT SELECT INTERACTION END ***"
    #   end
    #   if viewing_dialog_interactions.any? {|interaction| interaction.events.last == event}
    #     puts "*** VIEWING DIALOG INTERACTION END ***"
    #   end
    # end

    row = task.to_row

    puts row.join("\t")
    CSV_FILE.print(row.to_csv)
    # [
    #   participant_i,
    #   task_num,
    #   task_name,
    #   treatment,
    #   events_intact ? "yes" : "no",
    #   events.map(&:code_annotations_count).compact.last == 0 ? "yes" : "no",
    #   events.any?(&:task_timeout?) ? "yes" : "no",
    #   gross_time,
    #   interaction_end_time - interaction_start_time,
    #   text_select_interactions.count,
    #   text_select_interacting_time,
    #   text_select_interactions.flat_map(&:events).count(&:choose_deuce_exp?),
    #   text_select_interactions.flat_map(&:events).select(&:choose_deuce_exp?).count { |e| e.deuce_selections_count == 1 },
    #   text_select_interactions.flat_map(&:events).select(&:choose_deuce_exp?).count { |e| e.deuce_selections_count >= 2 },
    #   box_select_interactions.count,
    #   box_select_interacting_time,
    #   box_select_interactions.flat_map(&:events).count(&:choose_deuce_exp?),
    #   box_select_interactions.flat_map(&:events).select(&:choose_deuce_exp?).count { |e| e.deuce_selections_count == 1 },
    #   box_select_interactions.flat_map(&:events).select(&:choose_deuce_exp?).count { |e| e.deuce_selections_count >= 2 },
    #   all_interactions.count,
    #   interacting_time,
    #   invocations.size - invocations.grep(/^(Undo|Redo)$/).size,
    #   events.count(&:undo?),
    #   events.count(&:redo?),
    #   invocations.join("; "),
    # ].join("\t")
    # box_select_interactions.each do |interaction|
    #   interaction.events.each { |e| puts e.to_tsv }
    # end
  end
end

puts
puts [
  "Task",
  "Treatment",
  "Completion Rate",
  "Mean Time (completed tasks only)",
  "Mean Text-Select Refactorings (completed tasks only)",
  "Mean Text-Select Refactorings, Single Arg (completed tasks only)",
  "Mean Text-Select Refactorings, Multi Arg (completed tasks only)",
  "Mean Box-Select Refactorings (completed tasks only)",
  "Mean Box-Select Refactorings, Single Arg (completed tasks only)",
  "Mean Box-Select Refactorings, Multi Arg (completed tasks only)",
  "Mean Undos (completed only)",
  "Mean Redos (completed only)",
].join("\t")

all_tasks.group_by { |task| [task.name, task.treatment] }.sort_by(&:first).each do |(name, treatment),tasks|
  completed, incomplete = tasks.partition(&:completed?)
  puts [
    name,
    treatment,
    completed.count.to_f / tasks.count,
    completed.map(&:interaction_time).mean,
    completed.map(&:text_select_refactorings).map(&:count).mean,
    completed.map(&:single_arg_text_select_refactorings).map(&:count).mean,
    completed.map(&:multi_arg_text_select_refactorings).map(&:count).mean,
    completed.map(&:box_select_refactorings).map(&:count).mean,
    completed.map(&:single_arg_box_select_refactorings).map(&:count).mean,
    completed.map(&:multi_arg_box_select_refactorings).map(&:count).mean,
    completed.map(&:undo_count).mean,
    completed.map(&:redo_count).mean,
  ].join("\t")
end

# class Survey
#   attr_reader :json
#
#   def initialize(json_str)
#     @json = JSON.parse(json_str)
#   end
#
#   def years_experience
#     Float(json["experience"])
#   end
#
#   def years_functional_experience
#     Float(json["functional_experience"])
#   end
#
#   def other_refactoring_tools
#     json["prior_sns"]
#   end
#
#   def prior_sns
#     json["prior_sns"]
#   end
#
#   def prior_sns_code_tools
#     json["prior_sns_code_tools"]
#   end
#
#   def one_rectangle_mode_comparison
#     json["one_rectangle_mode_comparison"] && json["one_rectangle_mode_comparison"].to_i
#   end
#
#   def two_circles_mode_comparison
#     json["two_circles_mode_comparison"] && json["two_circles_mode_comparison"].to_i
#   end
#
#   def three_rectangles_mode_comparison
#     json["three_rectangles_mode_comparison"] && json["three_rectangles_mode_comparison"].to_i
#   end
#
#   def target_icon_mode_comparison
#     json["target_icon_mode_comparison"] && json["target_icon_mode_comparison"].to_i
#   end
#
#   def sus_score
#     (1..10).map do |i|
#       i%2 == 1 ? Integer(json["sus_#{i}"]) - 1 : 5 - Integer(json["sus_#{i}"])
#     end.sum * 2.5
#   end
#
#   def computer
#     json["computer"]
#   end
# end

def output_metrics_row(row)
  puts row.join("\t")
  METRICS_FILE.print(row.to_csv)
end

puts
output_metrics_row [
  "Metric", "Min", "-2 * Std Err", "Mean", "+2 * Std Err", "Max"
]

[
  :years_experience,
  :years_functional_experience,
  :one_rectangle_mode_comparison,
  :two_circles_mode_comparison,
  :three_rectangles_mode_comparison,
  :target_icon_mode_comparison,
  :sus_score
].each do |metric|
  values = all_surveys.map(&metric).compact
  output_metrics_row [
    metric,
    values.min,
    values.rough_confidence_interval.begin,
    values.mean,
    values.rough_confidence_interval.end,
    values.max
  ]
end

user_tutorial_times =
  user_events.map do |events|
    (events.find(&:next_step_head_to_head?).time - events.first.time) / 60.0
  end

output_metrics_row [
  "Tutorial Minutes",
  user_tutorial_times.min,
  user_tutorial_times.rough_confidence_interval.begin,
  user_tutorial_times.mean,
  user_tutorial_times.rough_confidence_interval.end,
  user_tutorial_times.max
]

user_task_times =
  user_events.map do |events|
    (events.select(&:user_study_next?).last.time - events.find(&:next_step_head_to_head?).time) / 60.0
  end

output_metrics_row [
  "Task Minutes",
  user_task_times.min,
  user_task_times.rough_confidence_interval.begin,
  user_task_times.mean,
  user_task_times.rough_confidence_interval.end,
  user_task_times.max
]

values = all_tasks.reject { |task| task.treatment == "CodeToolsOnly" }.map(&:completed?)
output_metrics_row [
  "Head-to-head task completion rate",
  "",
  values.bernoulli_rough_confidence_interval.begin,
  values.bernoulli_mean,
  values.bernoulli_rough_confidence_interval.end,
  "",
]

all_tasks.group_by { |task| task.name }.sort.each do |name,tasks|
  values = tasks.map(&:completed?)
  output_metrics_row [
    "#{name} completion rate",
    "",
    values.bernoulli_rough_confidence_interval.begin,
    values.bernoulli_mean,
    values.bernoulli_rough_confidence_interval.end,
    "",
  ]
end

all_tasks.reject { |task| task.treatment == "CodeToolsOnly" }.group_by { |task| task.second_encounter? ? 1 : 0 }.sort.each do |is_second_encounter,tasks|
  values = tasks.map(&:completed?)
  output_metrics_row [
    "#{is_second_encounter == 1 ? "Second" : "First"} encounter completion rate",
    "",
    values.bernoulli_rough_confidence_interval.begin,
    values.bernoulli_mean,
    values.bernoulli_rough_confidence_interval.end,
    "",
  ]
end

all_tasks.group_by { |task| [task.name, task.treatment] }.sort_by(&:first).each do |(name, treatment),tasks|
  values = tasks.map(&:completed?)
  output_metrics_row [
    "#{name} #{treatment} completion rate",
    "",
    values.bernoulli_rough_confidence_interval.begin,
    values.bernoulli_mean,
    values.bernoulli_rough_confidence_interval.end,
    "",
  ]
end

all_tasks.reject { |task| task.treatment == "CodeToolsOnly" }.group_by { |task| [task.name, task.second_encounter? ? 1 : 0] }.sort.each do |(name, is_second_encounter),tasks|
  # if name == "Two Circles"
  #   tasks.each do |task|
  #     puts [task.name, task.participant_i, task.number, task.second_encounter?, task.completed?, task.treatment].join("\t")
  #   end
  # end
  values = tasks.map(&:completed?)
  output_metrics_row [
    "#{name} #{is_second_encounter == 1 ? "second" : "first"} encounter completion rate",
    "",
    values.bernoulli_rough_confidence_interval.begin,
    values.bernoulli_mean,
    values.bernoulli_rough_confidence_interval.end,
    "",
  ]
end

all_tasks.reject { |task| task.treatment == "CodeToolsOnly" }.group_by { |task| [task.treatment, task.second_encounter? ? 1 : 0] }.sort.each do |(treatment, is_second_encounter),tasks|
  values = tasks.map(&:completed?)
  output_metrics_row [
    "#{treatment} #{is_second_encounter == 1 ? "second" : "first"} encounter completion rate",
    "",
    values.bernoulli_rough_confidence_interval.begin,
    values.bernoulli_mean,
    values.bernoulli_rough_confidence_interval.end,
    "",
  ]
end

all_tasks.reject { |task| task.treatment == "CodeToolsOnly" }.group_by { |task| [task.name, task.treatment, task.second_encounter? ? 1 : 0] }.sort.each do |(name, treatment, is_second_encounter),tasks|
  values = tasks.map(&:completed?)
  output_metrics_row [
    "#{name} #{treatment} #{is_second_encounter == 1 ? "second" : "first"} encounter completion rate",
    "",
    values.bernoulli_rough_confidence_interval.begin,
    values.bernoulli_mean,
    values.bernoulli_rough_confidence_interval.end,
    "",
  ]
end

all_tasks.group_by { |task| [task.name, task.treatment] }.sort_by(&:first).each do |(name, treatment),tasks|
  values = tasks.select(&:completed?).map(&:interaction_time)
  output_metrics_row [
    "#{name} #{treatment} time",
    values.min,
    values.rough_confidence_interval.begin,
    values.mean,
    values.rough_confidence_interval.end,
    values.max
  ]
end

all_tasks.group_by { |task| [task.name, task.treatment] }.sort_by(&:first).each do |(name, treatment),tasks|
  values = tasks.select(&:completed?).map(&:refactorings_count)
  output_metrics_row [
    "#{name} #{treatment} refactorings invoked",
    values.min,
    values.rough_confidence_interval.begin,
    values.mean,
    values.rough_confidence_interval.end,
    values.max
  ]
end

all_tasks.group_by { |task| [task.name, task.treatment] }.sort_by(&:first).each do |(name, treatment),tasks|
  values = tasks.select(&:completed?).map(&:undo_count)
  output_metrics_row [
    "#{name} #{treatment} undo count",
    values.min,
    values.rough_confidence_interval.begin,
    values.mean,
    values.rough_confidence_interval.end,
    values.max
  ]
end

puts
puts [
  "Metric", "YesCount", "NoCount"
].join("\t")

puts ["Any Functional Experience", all_surveys.map(&:years_functional_experience).count {|years| years > 0}, all_surveys.map(&:years_functional_experience).count {|years| years == 0}].join("\t")
puts ["Used Own Computer", all_surveys.map(&:computer).count("personal"), all_surveys.map(&:computer).count("provided")].join("\t")
puts ["Used Mouse", mouse_usage.count(true), mouse_usage.count(false)].join("\t")
puts ["Other Refactoring Tools", all_surveys.map(&:other_refactoring_tools).count("yes"), ""].join("\t")
puts ["Prior SnS Exposure", all_surveys.map(&:prior_sns).count("yes"), ""].join("\t")
puts ["Prior SnS Code Tools", all_surveys.map(&:prior_sns_code_tools).count("yes"), ""].join("\t")
