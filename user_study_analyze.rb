require "irb"
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
    parsed.is_a?(Hash) && parsed[key]
  end

  def deuce_selections_count
    json_info("deuceSelectionsCount") && json_info("deuceSelectionsCount").to_i
  end

  def user_study_next?
    name =~ /^(New: )?User Study Next/
  end

  def next_step_head_to_head?
    user_study_next? && info =~ /^(Step )?\(HeadToHeadTask/
  end

  def next_step_task_name
    info[/^(?:Step )?\(\w+,\("([^"]+)"\,\w+\)\)/, 1]
  end

  def next_step_task_treatment
    info[/^(?:Step )?\(\w+,\("[^"]+"\,(\w+)\)\)/, 1]
  end

  def deuce_exp_chosen
    name[/^Choose Deuce Exp "([\S\s]*)"/, 1]
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
      if event.deuce_selections_count && event.deuce_selections_count == 0 && !shift_depressed
        interactions << BoxSelectInteraction.new(current_interaction_events)
        current_interaction_events = nil
      else
        current_interaction_events << event
      end
    elsif pending_interaction_events
      if event.shift_down?
        pending_interaction_events = [event]
      elsif event.deuce_selections_count && event.deuce_selections_count > 0
        current_interaction_events = pending_interaction_events + [event]
        pending_interaction_events = nil
      else
        pending_interaction_events << event
      end
    else
      if event.shift_down?
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

    # Can't rely on leave events coming after hover events: can happen in same millisecond when moving mouse horizontally in the menu bar.
    # Add global counter in Elm code???
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

def split_into_viewing_dialog_interactions(events)
  interactions = []
  current_interaction_events = nil
  events.each do |event|
    if current_interaction_events
      current_interaction_events << event
      if event.close_dialog_box?
        interactions << DialogInteraction.new(current_interaction_events)
        current_interaction_events = nil
      elsif event.open_dialog_box?
        raise "nested dialog interactions!!"
      end
    elsif event.open_dialog_box?
      current_interaction_events = [event]
    elsif event.close_dialog_box?
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

puts [
  "Participant Number",
  "Task Number",
  "Task",
  "Treatment",
  "Gross Time",
  "Interaction Time",
  "#Interactions Started",
  "Interacting Time",
  "#Refactorings",
  "#Undo",
  "#Redo",
  "Invocations",
].join("\t")

participant_indices.each do |participant_i|
  events = File.read("#{LOGS_DIR}/participant_#{participant_i}.log").split("\n").map { |line| Event.new(line) }.sort_by(&:time) # Events are sent async and are occasionally slightly out of order

  head_to_head_tasks = split_into_head_to_head_tasks(events)

  task_num = 0

  head_to_head_tasks.each do |events|
    task_name = events[0].next_step_task_name
    treatment = events[0].next_step_task_treatment

    next if treatment == "ReadOnly"

    task_num += 1

    gross_start_time = events[0].time
    gross_end_time   = events[-1].time

    if treatment == "BoxSelectOnly" || treatment == "TextSelectOnly"
      # These should be mutually exclusive (TODO: check)
      interactions = split_into_box_select_interactions(events) + split_into_text_select_interactions(events)
      interactions.sort_by!(&:begin)
      interactions_count = interactions.count
      if interactions.size == 0
        interaction_start_time = 0
        interaction_end_time = 0
      else
        interaction_start_time = interactions.first.begin
        interaction_end_time   = interactions.last.end
      end
      viewing_dialog_interactions = split_into_viewing_dialog_interactions(events)
      interacting_time = (TimeRanges.new(interactions.map(&:time_range)) - TimeRanges.new(viewing_dialog_interactions.map(&:time_range))).duration
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
      task_num,
      task_name,
      treatment,
      gross_end_time - gross_start_time,
      interaction_end_time - interaction_start_time,
      interactions_count,
      interacting_time,
      invocations.size - invocations.grep(/^(Undo|Redo)$/).size,
      events.count(&:undo?),
      events.count(&:redo?),
      invocations.join("; "),
    ].join("\t")
  end

end