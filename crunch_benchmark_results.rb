require "pry"

RESULTS_PATH = "benchmark_results.csv"



# - Lines of codes for each example (ignore comments and blank lines)




#  /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --enable-logging=stderr --v=1 2>&1 >/dev/null | grep 'CONSOLE.*sns.js' | sed -e 's/^[^"]*"//g' -e 's/"[^"]*$//g' > benchmark_results.csv
#
# In web console: outputAllExampleStats();
#
# Close the web console. It will crash after 334,000 lines anyway.
#
# When Chrome no longer uses any CPU, quit chrome in the GUI or with cmd-Q (do
# NOT use cntl-c in the Terminal). We have to flush the buffers through sed
# properly so we don't drop lines. (The sed command line flag that supposedly
# makes it line buffered seems not to work in this case.)


CHROME_LOAD_TIMINGS_PATH  = "chrome_timings.csv"
FIREFOX_LOAD_TIMINGS_PATH = "firefox_timings.csv"

# For load time numbers:
#
# outputAllExampleTimings();


CHROME  = "Chrome/49.0.2623.87 Safari/537.36"
FIREFOX = "Gecko/20100101 Firefox/45.0"


EXAMPLES_WHITELIST = [
  "Wave Boxes",
  "Wave Boxes Grid",
  "Logo",
  "Botanic Garden Logo",
  "Active Trans Logo",
  "Sailboat",
  "Chicago Flag",
  "Sliders",
  "Buttons",
  "Widgets",
  "xySlider",
  "Tile Pattern",
  "Color Picker",
  "Ferris Wheel",
  "Ferris Task Before",
  "Ferris Task After",
  "Ferris Wheel Slideshow",
  "Survey Results",
  "Hilbert Curve Animation",
  "Bar Graph",
  "Pie Chart",
  "Solar System",
  "Clique",
  "Eye Icon",
  "Wikimedia Logo",
  "Haskell.org Logo",
  "Cover Logo",
  "POP-PL Logo",
  "Lillicon P",
  "Lillicon P, v2",
  "Keyboard",
  "Keyboard Task Before",
  "Keyboard Task After",
  "Tessellation Task Before",
  "Tessellation Task After",
  # "Tessellation 2",
  "Floral Logo 1",
  "Floral Logo 2",
  "Spiral Spiral-Graph",
  "Rounded Rect",
  "Thaw/Freeze",
  "3 Boxes",
  "N Boxes Sli",
  "N Boxes",
  "Elm Logo",
  "Logo 2",
  "Logo Sizes",
  "Rings",
  "Polygons",
  "Stars",
  "Triangles",
  "US-13 Flag",
  "US-50 Flag",
  "French Sudan Flag",
  "Frank Lloyd Wright",
  # "Frank Lloyd Wright B",
  "Bezier Curves",
  "Fractal Tree",
  "Stick Figures",
  # "Cult of Lambda",
  "Matrix Transformations",
  "Misc Shapes",
  "Interface Buttons",
  "Paths 1",
  "Paths 2",
  "Paths 3",
  "Paths 4",
  "Paths 5",
  "Sample Rotations",
  "Grid Tile",
  "Zones"
]

#
# def maybe_float_to_string(maybe_float)
#   if maybe_float
#     "%.2f" % maybe_float
#   else
#     "-"
#   end
# end
#

def tabular(table_data, header_rows = nil, col_config = nil)
  col_config ||= (header_rows || [[]]).select {|row| row.is_a?(Array)}.first.map { "c" }.join " | "
  str = ""
  str << "\\begin{tabular}{#{col_config}}\n"
  # if headers
  #   str << "\\hline\n"
  #   str << headers.map { |h| "\\textbf{#{h}}" }.join(" & ") << " \\\\\n"
  # end
  # str << "\\hline\n"
  ((header_rows || []) + table_data).each do |row|
    if row.respond_to?(:map)
      str << row.map(&:to_s).join(" & ") << " \\\\\n"
    else
      str << row << "\n"
    end
  end
  str << "\\end{tabular}\n"
  str
end

def latex_wrap(latex_body_str)
  <<-LATEX
%\\documentclass[10pt]{sigplanconf-pldi16}
\\documentclass[10pt]{article}

\\usepackage{amssymb,amsmath}
\\usepackage{amsthm}
\\usepackage{stmaryrd}
\\usepackage{hyperref}
\\usepackage{graphicx}
\\usepackage{color}
\\usepackage{fancyvrb}
\\usepackage{tikz}
%\\usepackage{../popl2016/mathpartir}
%\\usepackage{../popl2016/wrapfig}

\\usepackage{multirow}
\\usepackage{bm}
\\usepackage[top=1in, bottom=1in, left=1in, right=1in]{geometry}
%\\usepackage[parfill]{parskip}			% Activate to begin paragraphs with an empty line rather than an indent

%\\input{commands}

\\pagestyle{empty} % no page number

\\special{papersize=8.5in,11in}
\\setlength{\\pdfpageheight}{\\paperheight}
\\setlength{\\pdfpagewidth}{\\paperwidth}

\\begin{document}

body here

\\end{document}
  LATEX
  .sub("body here") { latex_body_str }
end




STR_DEDUP = {}

def dedup(str)
  STR_DEDUP[str] ||= str
end

class ResultRow
  def self.from_line(str)
    (browser, example_name, heuristics_mode, metric_name, value) = str.split(/,(?=\S)/)

    case value
    when /\A\s*-?[0-9]+\s*\z/   then value = value.to_i
    when /\A\s*-?[0-9\.]+\s*\z/ then value = value.to_f
    when /\A\s*True\s*\z/i      then value = true
    when /\A\s*False\s*\z/i     then value = false
    else                             value = value.chomp
    end

    new(
      browser:         browser,
      example_name:    example_name,
      heuristics_mode: heuristics_mode,
      metric_name:     metric_name,
      value:           value
    )
  end

  attr_reader :browser, :example_name, :heuristics_mode, :metric_name, :value

  def initialize(attrs)
    @browser         = dedup(attrs[:browser])
    @example_name    = dedup(attrs[:example_name])
    @heuristics_mode = dedup(attrs[:heuristics_mode])
    @metric_name     = dedup(attrs[:metric_name])
    @value           = dedup(attrs[:value])
  end

  def firefox?
    browser == FIREFOX
  end

  def chrome?
    browser == CHROME
  end
end

class Array
  def mean
    reduce(:+) / size.to_f
  end
end

class ExampleResults < Array
  attr_reader :example_name

  def initialize(example_name, result_rows)
    @example_name = example_name
    super(result_rows)
  end

  # def program_loc_count
  # end

  def output_loc_count
    find { |row| row.metric_name == "non-prelude locs in output count" }.value
  end

  def unfrozen_output_loc_count
    output_loc_count - find { |row| row.metric_name == "non-prelude frozen locs in output count" }.value
  end

  # ...,loc 88 (x0) frozen,False
  # ...,loc 88 (x0) trace count,12
  # ...,count of zones that might be able to change loc 88 (x0),48
  # ...,count of zones that chose to affect loc 88 (x0),24

  class Loc
    attr_reader :loc_id

    def initialize(loc_id, result_rows)
      @loc_id = loc_id
      @result_rows = result_rows
    end

    def frozen?
      @result_rows.find { |row| row.metric_name =~ / frozen\z/ }.value
    end

    def assigned_zones_count
      @result_rows.find { |row| row.metric_name =~ /\Acount of zones that chose to affect loc / }.value
    end

    def zones_that_could_have_assigned_loc_count
      @result_rows.find { |row| row.metric_name =~ /\Acount of zones that might be able to change loc / }.value
    end

    def assignment_rate
      assigned_zones_count / zones_that_could_have_assigned_loc_count.to_f
    end
  end

  def output_locs
    select do |row|
      row.metric_name =~ /\bloc (\d+)\b/
    end.group_by do |row|
      row.metric_name =~ /\bloc (\d+)\b/
      $1.to_i
    end.map do |loc_id, rows|
      Loc.new(loc_id, rows)
    end
  end

  def unfrozen_output_locs
    output_locs.reject(&:frozen?)
  end

  def never_assigned_loc_count
    unfrozen_output_locs.count { |loc| loc.assigned_zones_count == 0 }
  end

  def assigned_output_locs
    unfrozen_output_locs.select { |loc| loc.assigned_zones_count > 0 }
  end

  def assigned_loc_count
    assigned_output_locs.count
  end

  def assigned_loc_average_assignment_times
    assigned_output_locs.map(&:assigned_zones_count).reduce(:+) / assigned_output_locs.count.to_f
  end

  def assigned_loc_average_assignment_rate_among_possible_zones
    assigned_output_locs.map(&:assignment_rate).reduce(:+) / assigned_output_locs.count.to_f
  end

  def parse_results
    select { |row| row.metric_name == "parseE" }
  end

  def eval_results
    # simulate discarding the second result per code since these are only
    # supposed to be run once
    select { |row| row.metric_name == "Eval.run" }.each_with_index.select { |_, i| i%2 == 0 }.map(&:first)
  end

  def unparse_results
    select { |row| row.metric_name == "unparseE" }
  end

  def val_to_indexed_tree_results
    # simulate discarding the second result per code load since these are only
    # supposed to be run once
    select { |row| row.metric_name == "valToIndexedTree" }.each_with_index.select { |_, i| i%2 == 0 }.map(&:first)
  end

  def prepare_live_updates_results
    select { |row| row.metric_name == "prepareLiveUpdates" }
  end

  def run_code_results
    select { |row| row.metric_name == "Run" }
  end

  def ff_mean_parse_duration
    parse_results.select(&:firefox?).map(&:value).mean
  end

  def ff_mean_eval_duration
    eval_results.select(&:firefox?).map(&:value).mean
  end

  def ff_mean_unparse_duration
    unparse_results.select(&:firefox?).map(&:value).mean
  end

  def ff_mean_val_to_indexed_tree_duration
    val_to_indexed_tree_results.select(&:firefox?).map(&:value).mean
  end

  def ff_mean_prepare_live_updates_duration
    prepare_live_updates_results.select(&:firefox?).map(&:value).mean
  end

  def ff_mean_run_code_duration
    run_code_results.select(&:firefox?).map(&:value).mean
  end

  def chrome_mean_parse_duration
    parse_results.select(&:chrome?).map(&:value).mean
  end

  def chrome_mean_eval_duration
    eval_results.select(&:chrome?).map(&:value).mean
  end

  def chrome_mean_unparse_duration
    unparse_results.select(&:chrome?).map(&:value).mean
  end

  def chrome_mean_val_to_indexed_tree_duration
    val_to_indexed_tree_results.select(&:chrome?).map(&:value).mean
  end

  def chrome_mean_prepare_live_updates_duration
    prepare_live_updates_results.select(&:chrome?).map(&:value).mean
  end

  def chrome_mean_run_code_duration
    run_code_results.select(&:chrome?).map(&:value).mean
  end
end

result_rows = File.read(RESULTS_PATH).lines.map do |line|
  ResultRow.from_line(line)
end.select do |result_row|
  result_row.heuristics_mode == "Fair" &&
  EXAMPLES_WHITELIST.include?(result_row.example_name)
end.sort_by { |row| EXAMPLES_WHITELIST.index(row.example_name) }

by_browser = result_rows.group_by(&:browser)

single_browser_result_rows = by_browser[CHROME]

zone_summary_table_header_rows = [
  "\\hline",
  ["\\multirow{2}{*}{\\textbf{Example Name}}", "\\multirow{2}{*}{\\textbf{Shape Count}}", "\\multirow{2}{*}{\\textbf{Zone Count}}", "\\multicolumn{3}{c}{\\begin{minipage}{11em}\\textbf{\\vspace{0.1em}\\\\Number of zones with $0$, $1$, $>1$ loclist choices\\vspace{0.2em}}\\end{minipage}}"],
  "\\cline{4-6}",
  ["", "", "", "$\\bm{0}$", "$\\bm{1}$", "$\\bm{>1}$ \\textbf{(avg)}"],
  "\\hline \\hline",
]
zone_summary_table = []

total_example_count = 0
total_shape_count = 0
total_zone_count = 0
total_no_choices_count = 0
total_single_choice_count = 0
total_multiple_choices_count = 0
total_multiple_choices_choices_count = 0

single_browser_result_rows.group_by(&:example_name).each do |example_name, example_name_result_rows|

  shape_count = example_name_result_rows.find { |row| row.metric_name == "shape count" }.value
  zone_count  = example_name_result_rows.find { |row| row.metric_name == "zone count" }.value

  total_example_count += 1
  total_shape_count += shape_count
  total_zone_count += zone_count

  zone_loclist_count_rows = example_name_result_rows.select do |row|
    row.metric_name.include? "loclist options count (by SnS algorithm)"
  end

  no_choices_zones_count       = zone_loclist_count_rows.count  { |row| row.value == 0 }
  single_choice_zones_count    = zone_loclist_count_rows.count  { |row| row.value == 1 }
  multiple_choices_zones       = zone_loclist_count_rows.select { |row| row.value > 1 }
  multiple_choices_zones_count = multiple_choices_zones.size

  if multiple_choices_zones_count > 0
    multiple_choices_avg_count = multiple_choices_zones.map(&:value).reduce(:+) / multiple_choices_zones.size.to_f
  else
    multiple_choices_avg_count = nil
  end

  total_no_choices_count               += no_choices_zones_count
  total_single_choice_count            += single_choice_zones_count
  total_multiple_choices_count         += multiple_choices_zones_count
  total_multiple_choices_choices_count += multiple_choices_zones.map(&:value).reduce(:+) || 0

  zone_summary_table << [
    example_name,
    shape_count,
    zone_count,
    no_choices_zones_count,
    single_choice_zones_count,
    multiple_choices_zones_count.to_s + (multiple_choices_avg_count ? " (%.2f)" % multiple_choices_avg_count : "")
  ] << "\\hline"
end

zone_summary_table << "\\hline" << [
  "\\textbf{Totals}",
  total_shape_count,
  total_zone_count,
  total_no_choices_count,
  total_single_choice_count,
  total_multiple_choices_count.to_s + (" (%.2f)" % [total_multiple_choices_choices_count.to_f / total_multiple_choices_count])
] << "\\hline \\hline"

zone_summary_tables_latex =
  zone_summary_table.each_slice(80).map do |table_chunk|
    tabular(table_chunk, zone_summary_table_header_rows, "c c c c c c")
  end

results_by_example =
  single_browser_result_rows.group_by(&:example_name).map { |ex_name, rows| ExampleResults.new(ex_name, rows) }


assigned_locs =
  results_by_example.flat_map(&:assigned_output_locs)

loc_summary_table_header_rows = [
    "\\hline",
    [
      "\\textbf{Example Name}",
      "\\textbf{\\# Output Locs}",
      "\\textbf{Unfrozen}",
      "\\textbf{Unassigned}",
      "\\textbf{Assigned (avg times) (avg rate)}",
    ],
    "\\hline \\hline",
  ]

loc_summary_table =
  results_by_example.flat_map do |example_results|
    [
      [
        example_results.example_name,
        example_results.output_loc_count,
        example_results.unfrozen_output_loc_count,
        example_results.never_assigned_loc_count,
        example_results.assigned_loc_count.to_s + (example_results.assigned_loc_count > 0 ? " (%.1f) (%.0f\\%%)" % [example_results.assigned_loc_average_assignment_times, example_results.assigned_loc_average_assignment_rate_among_possible_zones * 100] : ""),
      ],
      "\\hline"
    ]
  end +
  [
    "\\hline",
    [
      "\\textbf{Totals}",
      results_by_example.map(&:output_loc_count).reduce(:+),
      results_by_example.map(&:unfrozen_output_loc_count).reduce(:+),
      results_by_example.map(&:never_assigned_loc_count).reduce(:+),
      results_by_example.map(&:assigned_loc_count).reduce(:+).to_s +
        " (%.1f) (%.0f\\%%)" % [
          assigned_locs.map(&:assigned_zones_count).reduce(:+) / assigned_locs.count.to_f,
          assigned_locs.map(&:assignment_rate).reduce(:+) / assigned_locs.count.to_f * 100.0
        ],
    ],
    "\\hline \\hline",
  ]


loc_summary_tables_latex =
  loc_summary_table.each_slice(80).map do |table_chunk|
    tabular(table_chunk, loc_summary_table_header_rows, "c c c c c")
  end



total_equations_count =
  single_browser_result_rows.count { |result_row| result_row.metric_name =~ /\Ashape .+ chosen loc\z/  && result_row.value !~ /none/ }

total_equations_count_2 =
  single_browser_result_rows.count { |result_row| result_row.metric_name =~ /\Ashape .+ trace size\z/ }

if total_equations_count != total_equations_count_2
  raise "Equation counts don't match!! (#{total_equations_count} vs #{total_equations_count_2}) This may mean that the output was cut off. Be sure to quit Chrome through the GUI."
end

# trace_size_sum =
#   single_browser_result_rows.
#     select { |result_row| result_row.metric_name =~ /\Ashape .+ trace size\z/ }.
#     map(&:value).
#     reduce(:+)

# per zone-attr; so traces may be double counted
# mean_trace_size = trace_size_sum / total_equations_count.to_f

# solve_durations =
#   single_browser_result_rows.
#     select { |result_row| result_row.metric_name =~ /\Ashape .+ attempt duration\z/ }.
#     map(&:value)

# max_time_to_solve = solve_durations.max

# mean_time_to_solve = solve_durations.reduce(:+) / solve_durations.size.to_f

EQUATION_ROW_METRIC_NAME_REGEXP = /\Ashape (.+) (?:trace size|trace id|chosen loc|\+= \d+ solve [AB] (?:attempt duration|solved\?))\z/

def shape_zone_attr(metric_name)
  metric_name =~ EQUATION_ROW_METRIC_NAME_REGEXP
  $1
end

class EquationResult
  attr_reader :example_name
  attr_reader :result_rows

  def initialize(example_name, result_rows)
    @example_name    = example_name
    @result_rows     = result_rows
  end

  def chosen_loc_and_trace_id
    [chosen_loc, trace_id]
  end

  def chosen_loc
    @result_rows.find { |row| row.metric_name =~ / chosen loc\z/ }.value
  end

  def trace_id
    @result_rows.find { |row| row.metric_name =~ / trace id\z/ }.value
  end

  def trace_size
    @result_rows.find { |row| row.metric_name =~ / trace size\z/ }.value
  end

  def solve_a_durations
    @result_rows.select { |row| row.metric_name =~ / solve A attempt duration\z/ }.map(&:value)
  end

  def solve_b_durations
    @result_rows.select { |row| row.metric_name =~ / solve B attempt duration\z/ }.map(&:value)
  end

  def solve_durations
    solve_a_durations + solve_b_durations
  end

  def in_solve_a_fragment?
    solve_a_durations.any?
  end

  def in_solve_b_fragment?
    solve_b_durations.any?
  end

  def in_solve_a_or_b_fragment?
    in_solve_a_fragment? || in_solve_b_fragment?
  end

  def plus1_solve_a_solved?
    @result_rows.find { |row| row.metric_name =~ / \+= 1 solve A solved\?\z/ && row.value == true }
  end

  def plus100_solve_a_solved?
    @result_rows.find { |row| row.metric_name =~ / \+= 100 solve A solved\?\z/ && row.value == true }
  end

  def plus1_solve_b_solved?
    @result_rows.find { |row| row.metric_name =~ / \+= 1 solve B solved\?\z/ && row.value == true }
  end

  def plus100_solve_b_solved?
    @result_rows.find { |row| row.metric_name =~ / \+= 100 solve B solved\?\z/ && row.value == true }
  end

  def plus1_solve_a_or_b_solved?
    plus1_solve_a_solved? || plus1_solve_b_solved?
  end

  def plus100_solve_a_or_b_solved?
    plus100_solve_a_solved? || plus100_solve_b_solved?
  end
end

equations =
  single_browser_result_rows.
    select { |result_row| result_row.metric_name =~ EQUATION_ROW_METRIC_NAME_REGEXP }.
    reject { |result_row| result_row.metric_name =~ / chosen loc\z/ && result_row.value == "none" }. # such zones have no corresponding solve rows
    group_by { |result_row| [result_row.example_name, result_row.heuristics_mode, shape_zone_attr(result_row.metric_name)] }.
    map do |(example_name, heuristics_mode, shape_zone_attr), result_rows|
      EquationResult.new(
        example_name,
        result_rows
      )
    end

equations_by_chosen_loc_and_trace_id =
  equations.
    group_by { |eqn| [eqn.example_name, eqn.chosen_loc_and_trace_id] }.
    map do |(example_name, (choose_loc, trace_id)), equation_results|
      EquationResult.new(
        example_name,
        equation_results.flat_map(&:result_rows)
      )
    end

# traces_in_solve_a_fragment_count =
#   single_browser_result_rows.
#     count { |result_row| result_row.metric_name =~ /\Ashape .+ \+= 1 solve A solved\?\z/ }
#
# traces_in_solve_b_fragment_count =
#   single_browser_result_rows.
#     count { |result_row| result_row.metric_name =~ /\Ashape .+ \+= 1 solve A solved\?\z/ }

# trace_size_by_example_headers = [
#   "\\hline",
#   ["\\textbf{Example Name}", "\\textbf{Mean Trace Size}"],
#   "\\hline",
# ]
#
# trace_size_by_example_table = [
#   *EXAMPLES_WHITELIST.map do |example_name|
#     equations_for_example = equations.select { |eqn_result| eqn_result.example_name == example_name }
#     [example_name, "%.2f" % equations_for_example.map(&:trace_size).mean]
#   end,
# ]
#
# trace_size_by_example_tables_latex =
#   trace_size_by_example_table.each_slice(40).map do |table_chunk|
#     tabular(table_chunk + ["\\hline"], trace_size_by_example_headers, "c c")
#   end


# equation_summary_table = [
#     "\\hline",
#     ["\\textbf{\\# Equations}", total_equations_count],
#     # ["\\textbf{\\# Equations 2}", total_equations_count_2],
#     ["\\textbf{Mean Trace Size (tree nodes)}", "%.2f" % mean_trace_size],
#     ["\\textbf{Max Solve Time}", "%.3f" % max_time_to_solve],
#     ["\\textbf{Mean Solve Time}",  "$< 0.001$ (Calculated: %.4f)" % mean_time_to_solve],
#     ["\\textbf{\\# Traces in Solve A fragment}", equations.count(&:in_solve_a_fragment?)],
#     ["\\textbf{\\hspace{2em}Solved}",            ""],
#     ["\\textbf{\\hspace{4em}$+= 1$}",            equations.select(&:in_solve_a_fragment?).count(&:plus1_solve_a_solved?)],
#     ["\\textbf{\\hspace{4em}$+= 100$}",          equations.select(&:in_solve_a_fragment?).count(&:plus100_solve_a_solved?)],
#     ["\\textbf{\\hspace{2em}Not Solved}",        ""],
#     ["\\textbf{\\hspace{4em}$+= 1$}",            equations.select(&:in_solve_a_fragment?).reject(&:plus1_solve_a_solved?).count],
#     ["\\textbf{\\hspace{4em}$+= 100$}",          equations.select(&:in_solve_a_fragment?).reject(&:plus100_solve_a_solved?).count],
#     ["\\textbf{\\# Traces in Solve B fragment}", equations.count(&:in_solve_b_fragment?)],
#     ["\\textbf{\\hspace{2em}Solved}",            ""],
#     ["\\textbf{\\hspace{4em}$+= 1$}",            equations.select(&:in_solve_b_fragment?).count(&:plus1_solve_b_solved?)],
#     ["\\textbf{\\hspace{4em}$+= 100$}",          equations.select(&:in_solve_b_fragment?).count(&:plus100_solve_b_solved?)],
#     ["\\textbf{\\hspace{2em}Not Solved}",        ""],
#     ["\\textbf{\\hspace{4em}$+= 1$}",            equations.select(&:in_solve_b_fragment?).reject(&:plus1_solve_b_solved?).count],
#     ["\\textbf{\\hspace{4em}$+= 100$}",          equations.select(&:in_solve_b_fragment?).reject(&:plus100_solve_b_solved?).count],
#     ["\\textbf{\\# Traces in either fragment}",  equations.count(&:in_solve_a_or_b_fragment?)],
#     ["\\textbf{\\hspace{2em}Solved}",            ""],
#     ["\\textbf{\\hspace{4em}$+= 1$}",            equations.select(&:in_solve_a_or_b_fragment?).count(&:plus1_solve_a_or_b_solved?)],
#     ["\\textbf{\\hspace{4em}$+= 100$}",          equations.select(&:in_solve_a_or_b_fragment?).count(&:plus100_solve_a_or_b_solved?)],
#     ["\\textbf{\\hspace{2em}Not Solved}",        ""],
#     ["\\textbf{\\hspace{4em}$+= 1$}",            equations.select(&:in_solve_a_or_b_fragment?).reject(&:plus1_solve_a_or_b_solved?).count],
#     ["\\textbf{\\hspace{4em}$+= 100$}",          equations.select(&:in_solve_a_or_b_fragment?).reject(&:plus100_solve_a_or_b_solved?).count],
#     ["\\textbf{\\# Traces in no fragment}",      equations.reject(&:in_solve_a_or_b_fragment?).count],
#     "\\hline",
#     # ["\\multicolumn{2}{l}{\\textbf{Some equations solved $+= 1$ but not $+= 100$:}}"],
#     # *(equations.select(&:plus1_solve_a_solved?).reject(&:plus100_solve_a_solved?).sample(25).map { |eq| [eq.example_name, eq.shape_zone_attr] }.sort),
#     # "\\hline",
#   ]
#
# equation_summary_table_latex =
#   tabular(equation_summary_table, nil, "l c")



equations_by_chosen_loc_and_trace_id_summary_table = [
    "\\hline",
    ["\\textbf{\\# Equations}", total_equations_count_2],
    ["\\textbf{\\# (example, loc, tr) Equations}", equations_by_chosen_loc_and_trace_id.count],
    ["\\textbf{Mean Trace Size (tree nodes)}", "%.2f" % equations_by_chosen_loc_and_trace_id.map(&:trace_size).mean],
    ["\\textbf{Max Solve Time}", "%.3f" % equations_by_chosen_loc_and_trace_id.flat_map(&:solve_durations).max],
    ["\\textbf{Mean Solve Time}",  "$< 0.001$ (Calculated: %.4f)" % equations_by_chosen_loc_and_trace_id.flat_map(&:solve_durations).mean],
    ["\\textbf{\\# Traces in Solve A fragment}", equations_by_chosen_loc_and_trace_id.count(&:in_solve_a_fragment?)],
    ["\\textbf{\\hspace{2em}Solved}",            ""],
    ["\\textbf{\\hspace{4em}$+= 1$}",            equations_by_chosen_loc_and_trace_id.select(&:in_solve_a_fragment?).count(&:plus1_solve_a_solved?)],
    ["\\textbf{\\hspace{4em}$+= 100$}",          equations_by_chosen_loc_and_trace_id.select(&:in_solve_a_fragment?).count(&:plus100_solve_a_solved?)],
    ["\\textbf{\\hspace{2em}Not Solved}",        ""],
    ["\\textbf{\\hspace{4em}$+= 1$}",            equations_by_chosen_loc_and_trace_id.select(&:in_solve_a_fragment?).reject(&:plus1_solve_a_solved?).count],
    ["\\textbf{\\hspace{4em}$+= 100$}",          equations_by_chosen_loc_and_trace_id.select(&:in_solve_a_fragment?).reject(&:plus100_solve_a_solved?).count],
    ["\\textbf{\\# Traces in Solve B fragment}", equations_by_chosen_loc_and_trace_id.count(&:in_solve_b_fragment?)],
    ["\\textbf{\\hspace{2em}Solved}",            ""],
    ["\\textbf{\\hspace{4em}$+= 1$}",            equations_by_chosen_loc_and_trace_id.select(&:in_solve_b_fragment?).count(&:plus1_solve_b_solved?)],
    ["\\textbf{\\hspace{4em}$+= 100$}",          equations_by_chosen_loc_and_trace_id.select(&:in_solve_b_fragment?).count(&:plus100_solve_b_solved?)],
    ["\\textbf{\\hspace{2em}Not Solved}",        ""],
    ["\\textbf{\\hspace{4em}$+= 1$}",            equations_by_chosen_loc_and_trace_id.select(&:in_solve_b_fragment?).reject(&:plus1_solve_b_solved?).count],
    ["\\textbf{\\hspace{4em}$+= 100$}",          equations_by_chosen_loc_and_trace_id.select(&:in_solve_b_fragment?).reject(&:plus100_solve_b_solved?).count],
    ["\\textbf{\\# Traces in either fragment}",  equations_by_chosen_loc_and_trace_id.count(&:in_solve_a_or_b_fragment?)],
    ["\\textbf{\\hspace{2em}Solved}",            ""],
    ["\\textbf{\\hspace{4em}$+= 1$}",            equations_by_chosen_loc_and_trace_id.select(&:in_solve_a_or_b_fragment?).count(&:plus1_solve_a_or_b_solved?)],
    ["\\textbf{\\hspace{4em}$+= 100$}",          equations_by_chosen_loc_and_trace_id.select(&:in_solve_a_or_b_fragment?).count(&:plus100_solve_a_or_b_solved?)],
    ["\\textbf{\\hspace{2em}Not Solved}",        ""],
    ["\\textbf{\\hspace{4em}$+= 1$}",            equations_by_chosen_loc_and_trace_id.select(&:in_solve_a_or_b_fragment?).reject(&:plus1_solve_a_or_b_solved?).count],
    ["\\textbf{\\hspace{4em}$+= 100$}",          equations_by_chosen_loc_and_trace_id.select(&:in_solve_a_or_b_fragment?).reject(&:plus100_solve_a_or_b_solved?).count],
    ["\\textbf{\\# Traces in no fragment}",      equations_by_chosen_loc_and_trace_id.reject(&:in_solve_a_or_b_fragment?).count],
    "\\hline",
    # ["\\multicolumn{2}{l}{\\textbf{Some equations solved $+= 1$ but not $+= 100$:}}"],
    # *(equations.select(&:plus1_solve_a_solved?).reject(&:plus100_solve_a_solved?).sample(25).map { |eq| [eq.example_name, eq.shape_zone_attr] }.sort),
    # "\\hline",
  ]


equations_by_chosen_loc_and_trace_id_summary_table_latex =
  tabular(equations_by_chosen_loc_and_trace_id_summary_table, nil, "l c")


timing_result_rows =
  (File.read(CHROME_LOAD_TIMINGS_PATH) + File.read(FIREFOX_LOAD_TIMINGS_PATH)).lines.map do |line|
    ResultRow.from_line(line)
  end.select do |result_row|
    result_row.heuristics_mode == "Fair" &&
    EXAMPLES_WHITELIST.include?(result_row.example_name)
  end.sort_by { |row| EXAMPLES_WHITELIST.index(row.example_name) }

timing_results_by_example =
  timing_result_rows.group_by(&:example_name).map { |ex_name, rows| ExampleResults.new(ex_name, rows) }

timing_summary_table_header_rows = [
    "\\hline",
    [
      "\\textbf{Example Name}",
      "\\textbf{Parse}",
      "\\textbf{Eval}",
      "\\textbf{Unparse}",
      "\\textbf{valToInde...}",
      "\\textbf{prepareLi...}",
      "\\textbf{``Run Code''}",
    ],
    "\\hline \\hline",
  ]

timing_summary_table =
  timing_results_by_example.flat_map do |example_results|
    [
      [
        "\\multirow{2}{*}{#{example_results.example_name}}",
        "FF: %.3f" % example_results.ff_mean_parse_duration,
        "FF: %.3f" % example_results.ff_mean_eval_duration,
        "FF: %.3f" % example_results.ff_mean_unparse_duration,
        "FF: %.3f" % example_results.ff_mean_val_to_indexed_tree_duration,
        "FF: %.3f" % example_results.ff_mean_prepare_live_updates_duration,
        "FF: %.3f" % example_results.ff_mean_run_code_duration,
      ],
      [
        "",
        "Ch: %.3f" % example_results.chrome_mean_parse_duration,
        "Ch: %.3f" % example_results.chrome_mean_eval_duration,
        "Ch: %.3f" % example_results.chrome_mean_unparse_duration,
        "Ch: %.3f" % example_results.chrome_mean_val_to_indexed_tree_duration,
        "Ch: %.3f" % example_results.chrome_mean_prepare_live_updates_duration,
        "Ch: %.3f" % example_results.chrome_mean_run_code_duration,
      ],
      "\\hline"
    ]
  end +
  [
    "\\hline",
    [
      "\\multirow{5}{*}{\\textbf{Summary}}",
      "\\textbf{FF: %.3f}" % ExampleResults.new("All", timing_result_rows).parse_results.select(&:firefox?).map(&:value).mean,
      "\\textbf{FF: %.3f}" % ExampleResults.new("All", timing_result_rows).eval_results.select(&:firefox?).map(&:value).mean,
      "\\textbf{FF: %.3f}" % ExampleResults.new("All", timing_result_rows).unparse_results.select(&:firefox?).map(&:value).mean,
      "\\textbf{FF: %.3f}" % ExampleResults.new("All", timing_result_rows).val_to_indexed_tree_results.select(&:firefox?).map(&:value).mean,
      "\\textbf{FF: %.3f}" % ExampleResults.new("All", timing_result_rows).prepare_live_updates_results.select(&:firefox?).map(&:value).mean,
      "\\textbf{FF: %.3f}" % ExampleResults.new("All", timing_result_rows).run_code_results.select(&:firefox?).map(&:value).mean,
    ],
    [
      "",
      "\\textbf{Ch: %.3f}" % ExampleResults.new("All", timing_result_rows).parse_results.select(&:chrome?).map(&:value).mean,
      "\\textbf{Ch: %.3f}" % ExampleResults.new("All", timing_result_rows).eval_results.select(&:chrome?).map(&:value).mean,
      "\\textbf{Ch: %.3f}" % ExampleResults.new("All", timing_result_rows).unparse_results.select(&:chrome?).map(&:value).mean,
      "\\textbf{Ch: %.3f}" % ExampleResults.new("All", timing_result_rows).val_to_indexed_tree_results.select(&:chrome?).map(&:value).mean,
      "\\textbf{Ch: %.3f}" % ExampleResults.new("All", timing_result_rows).prepare_live_updates_results.select(&:chrome?).map(&:value).mean,
      "\\textbf{Ch: %.3f}" % ExampleResults.new("All", timing_result_rows).run_code_results.select(&:chrome?).map(&:value).mean,
    ],
    [
      "",
      "\\textbf{Min %.3f}" % ExampleResults.new("All", timing_result_rows).parse_results.map(&:value).min,
      "\\textbf{Min %.3f}" % ExampleResults.new("All", timing_result_rows).eval_results.map(&:value).min,
      "\\textbf{Min %.3f}" % ExampleResults.new("All", timing_result_rows).unparse_results.map(&:value).min,
      "\\textbf{Min %.3f}" % ExampleResults.new("All", timing_result_rows).val_to_indexed_tree_results.map(&:value).min,
      "\\textbf{Min %.3f}" % ExampleResults.new("All", timing_result_rows).prepare_live_updates_results.map(&:value).min,
      "\\textbf{Min %.3f}" % ExampleResults.new("All", timing_result_rows).run_code_results.map(&:value).min,
    ],
    [
      "",
      "\\textbf{Avg %.3f}" % ExampleResults.new("All", timing_result_rows).parse_results.map(&:value).mean,
      "\\textbf{Avg %.3f}" % ExampleResults.new("All", timing_result_rows).eval_results.map(&:value).mean,
      "\\textbf{Avg %.3f}" % ExampleResults.new("All", timing_result_rows).unparse_results.map(&:value).mean,
      "\\textbf{Avg %.3f}" % ExampleResults.new("All", timing_result_rows).val_to_indexed_tree_results.map(&:value).mean,
      "\\textbf{Avg %.3f}" % ExampleResults.new("All", timing_result_rows).prepare_live_updates_results.map(&:value).mean,
      "\\textbf{Avg %.3f}" % ExampleResults.new("All", timing_result_rows).run_code_results.map(&:value).mean,
    ],
    [
      "",
      "\\textbf{Max %.3f}" % ExampleResults.new("All", timing_result_rows).parse_results.map(&:value).max,
      "\\textbf{Max %.3f}" % ExampleResults.new("All", timing_result_rows).eval_results.map(&:value).max,
      "\\textbf{Max %.3f}" % ExampleResults.new("All", timing_result_rows).unparse_results.map(&:value).max,
      "\\textbf{Max %.3f}" % ExampleResults.new("All", timing_result_rows).val_to_indexed_tree_results.map(&:value).max,
      "\\textbf{Max %.3f}" % ExampleResults.new("All", timing_result_rows).prepare_live_updates_results.map(&:value).max,
      "\\textbf{Max %.3f}" % ExampleResults.new("All", timing_result_rows).run_code_results.map(&:value).max,
    ],
    "\\hline \\hline",
  ]


timing_summary_tables_latex =
  timing_summary_table.each_slice(72).map do |table_chunk|
    tabular(table_chunk, timing_summary_table_header_rows, "c c c c c c c")
  end

File.write(
  "benchmark_table.tex",
  latex_wrap(
    zone_summary_tables_latex.join("\n\n\\vspace{3em}\n\n") +
    "\n\n\\vspace{6em}\n\n" +
    loc_summary_tables_latex.join("\n\n\\vspace{3em}\n\n") +
    "\n\n\\vspace{6em}\n\n" +
    # trace_size_by_example_tables_latex.join("\n\n\\vspace{3em}\n\n") +
    # "\n\n\\vspace{6em}\n\n" +
    equations_by_chosen_loc_and_trace_id_summary_table_latex +
    "\n\n\\vspace{6em}\n\n" +
    # equation_summary_table_latex +
    # "\n\n\\vspace{6em}\n\n" +
    timing_summary_tables_latex.join("\n\n\\vspace{3em}\n\n")
  )
)

system "pdflatex benchmark_table.tex"
system "open benchmark_table.pdf"


# Are we capturing all dimensions of interactivity?
#   - Percentage of output locs never assigned under each mode
#
# How often does interacting with a zone produce a response?