require "pry"

RESULTS_PATH = "benchmark_results.csv"

CHROME = "Chrome/49.0.2623.87 Safari/537.36"

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
  "Tessellation 2",
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
  "Frank Lloyd Wright B",
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
end

class ExampleResults < Array
  attr_reader :example_name

  def initialize(example_name, result_rows)
    @example_name = example_name
    super(result_rows)
  end

  def program_loc_count
    0
  end

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
end

result_rows = File.read(RESULTS_PATH).lines.map do |line|
  ResultRow.from_line(line)
end.select do |result_row|
  result_row.heuristics_mode == "Fair" &&
  EXAMPLES_WHITELIST.include?(result_row.example_name)
end.reverse

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

mean_trace_size = nil # per zone-attr; so traces may be double counted
max_time_to_solve = nil
mean_time_to_solve = nil
traces_not_in_solvable_subset = nil
traces_in_solvable_subset = nil
traces_in_solvable_subset_solved = nil
traces_in_solvable_subset_not_solved = nil


File.write(
  "benchmark_table.tex",
  latex_wrap(
    zone_summary_tables_latex.join("\n\n\\vspace{3em}\n\n") +
    "\n\n\\vspace{6em}\n\n" +
    loc_summary_tables_latex.join("\n\n\\vspace{3em}\n\n")
  )
)

system "pdflatex benchmark_table.tex"
system "open benchmark_table.pdf"


# Are we capturing all dimensions of interactivity?
#   - Percentage of output locs never assigned under each mode
#
# How often does interacting with a zone produce a response?