require 'pathname'
require 'tmpdir'
require './helper/format-output'

task :default => :build
K_BACKEND = "java"

KORO_TIMESTAMP = "oro-kompiled/timestamp"

FileList['*.k'].each do |src|
  file KORO_TIMESTAMP => src
end

file KORO_TIMESTAMP do
  #env = ENV.to_h.merge('K_OPTS' => "-Xmx2048m")
  env = {'K_OPTS' => '-Xmx2048m'}
  sh env, "kompile --backend #{K_BACKEND} oro.k"
end

def oro_parser_stack
  Pathname(__FILE__).dirname + "oro-parser" + "stack.yaml"
end

desc "Print path to the parser's stack.yaml"
task :parser_stack do
  puts oro_parser_stack
end

def oro_parser_exe
  `stack --stack-yaml #{oro_parser_stack} exec -- which oro-parser-exe`.chomp
end

file oro_parser_exe => FileList['oro-parser/**/*.hs'].exclude(%r{/test/}).to_a do |t|
  sh %Q{stack --stack-yaml #{oro_parser_stack} build}
end

namespace :build do
  desc "Build the Oro YAML -> kast tool (./parser.sh)"
  task :parser => [oro_parser_exe]
end

desc "Build the Oro executable semantic interpreter"
task :build => [KORO_TIMESTAMP, 'build:parser']

namespace :test do
  def test_inputs
    FileList['test/**/*.oro'].exclude('test/expected_output/**/*').to_a
  end

  def expected_output_path_of test_file
    Pathname(__FILE__).dirname + "test" + "expected_output" + (
      Pathname(test_file).relative_path_from Pathname("test")
    )
  end

  def run_test(test_file, quiet: false)
    expected_outpath = expected_output_path_of test_file
    Dir.mktmpdir do |dir|
      dir = Pathname(dir)
      actual_outpath = dir + 'actual.txt'
      sh './koro-run', '--output-file', actual_outpath.to_s, test_file
      actual_output = ConfigStyler.style(actual_outpath.read)
      actual_outpath.write actual_output
      unless quiet
        puts
        puts actual_output
        puts
      end
      if expected_outpath.exist?
        expected_output = expected_outpath.read
        if actual_output == expected_output
          yield :match, nil, nil
        else
          yield :mismatch, expected_outpath, actual_outpath
        end
      else
        yield :no_expectation, expected_outpath, actual_output
      end
    end
  end

  desc "Test one case"
  task :one => [:build] do
    require 'highline'  # <- highline gem
    last_pick = Pathname(__FILE__).dirname + ".last_one_test"
    cli = HighLine.new
    begin
      test_file = cli.choose(*test_inputs) do |menu|
        menu.header = "\nAvailable Tests"
        menu.prompt = "Test to run? "
        if last_pick.exist?
          defval = last_pick.read.strip
          test_inputs.index(defval).tap do |defidx|
            break if defidx.nil?
            menu.default = (defidx + 1).to_s
            menu.prompt = "or a different test? "
          end
        end
      end
    rescue Interrupt
      puts "\n!!! User aborted !!!"
      exit 1
    end
    last_pick.write test_file

    expected_outpath = expected_output_path_of test_file
    run_test(test_file) do |outcome, *args|
      case outcome
      when :match
        cli.say cli.color("*** Output matched expected ***", :bright_green)

      when :mismatch
        tool = DiffTool.load_from(Pathname(__FILE__).dirname)
        if cli.agree(cli.color("Output does not match expected; #{tool.verb} #{tool.name}? ", :red), true)
          tool.run(*args)
        else
          exit 1
        end

      when :no_expectation
        expected_outpath, actual_output = args
        if cli.agree(cli.color("No expected output provided; save this as expected? ", :yellow), true)
          expected_outpath.dirname.mkpath
          expected_outpath.write actual_output
        end
      end
    end
  end

  desc "Test all cases with expected output"
  task :all => [:build] do
    begin
      require 'highline'
      cli = HighLine.new
      output_result = ->(desc, color) {cli.say cli.color(desc, color)}
    rescue
      output_result = ->(desc, color) {puts desc}
    end

    this_dir = Pathname(__FILE__).dirname
    expected_output_files = Set.new FileList["test/expected_output/**/*.oro"]
    all_passed = true
    test_inputs.each do |test_file|
      expected_outpath = expected_output_path_of test_file
      next unless expected_output_files.find {|ofp| expected_outpath.to_s.end_with? ofp}
      run_test(test_file, quiet: true) do |outcome, *args|
        all_passed = false unless outcome == :match
        result_desc, color = {
          match: ["ok", :bright_green],
          mismatch: ["failed", :red],
        }[outcome] || ["unknown result: #{outcome}", :red]
        output_result["    ^^^ #{result_desc} ^^^", color]
      end
    end

    unless all_passed
      output_result[%Q{\nSome tests failed.  Run "rake test:one" for more details on specific failure.\n}, :red]
      exit 1
    end
  end

  desc "Set diff tool for testing"
  task :set_differ, [:tool] do |t, args|
    raise "'tool' argument required" unless args.tool
    DiffTool.set_in(Pathname(__FILE__).dirname, args.tool)
  end
end

class DiffTool
  TOOLS = {
    'diff' => {tool: 'diff', interactive: false},
    'meld' => {tool: 'meld', interactive: true},
  }

  def self.tool_spec_file(dir)
    Pathname(dir) + ".test_diff_tool"
  end

  def self.load_from(dir)
    tsfile = tool_spec_file(dir)
    tool_name = (
      if tsfile.exist?
        tsfile.read.strip
      else
        'diff'
      end
    )
    new(TOOLS[tool_name] || TOOLS['diff'])
  end

  def self.set_in(dir, tool_name)
    raise ArgumentError, "'#{tool_name}' unknown" unless TOOLS.has_key?(tool_name)
    tool_spec_file(dir).write tool_name
  end

  def initialize(tool: , interactive: )
    @tool = tool
    @interactive = !!interactive
  end

  attr_reader :tool

  def interactive?
    @interactive
  end

  def verb
    interactive? ? 'launch' : 'run'
  end

  def name
    @tool
  end

  # Paths are expected -> actual
  def run(*paths)
    if interactive?
      launch_in_background(*paths)
    else
      ConsoleLoop.new(@tool, *paths).run
    end
  end

  class ConsoleLoop
    def initialize(tool, expected_path, actual_path)
      @tool = tool
      @expected = expected_path
      @actual = actual_path
      @cli = HighLine.new
      @next_action = method(:show_diff)
      @editor = (ENV['DISPLAY'] && ENV['VISUAL']) || ENV['EDITOR'] || 'vi'
    end

    def run
      while @next_action
        action, @next_action = @next_action, nil
        action.call
      end
    end

    def show_diff
      system(@tool, @expected.to_s, @actual.to_s)
      show_menu
    end

    def show_menu
      @cli.choose do |menu|
        menu.header = "\nAvailable Actions"
        menu.choice('Compare again') {next_do :show_diff}
        menu.choice('Accept actual as expected') {next_do :accept_actual}
        menu.choice("Edit expected with '#{@editor}'") {next_do :edit_expected}
        menu.choice('Select a different editor') {next_do :select_editor}
        menu.choice('Quit')
        menu.prompt = "Proceed by: "
      end

    end

    def accept_actual
      @expected.write @actual.read
    end

    def edit_expected
      system(@editor, @expected.to_s)
      show_menu
    end

    def select_editor
      @editor = @cli.ask("Editor to use: ") do |q|
        q.validate = proc {|a| raise ArgumentError if `which #{a}` == ''; true}
      end
      show_menu
    rescue ArgumentError
      retry
    rescue Interrupt
      show_menu
    end

    private
    def next_do(meth_name)
      @next_action = method(meth_name)
    end
  end

  private
  def launch_in_background(expected, actual)
    actual.open do |f|
      fd_n = 10
      Process.detach spawn(@tool, expected.to_s, "/proc/self/fd/#{fd_n}", {fd_n => f.fileno})
    end
  end
end
