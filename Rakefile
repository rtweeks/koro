require 'pathname'
require 'tmpdir'

task :default => :build
K_BACKEND = "java"

KORO_TIMESTAMP = "oro-kompiled/timestamp"

FileList['*.k'].each do |src|
  file KORO_TIMESTAMP => src
end

file KORO_TIMESTAMP do
  sh "kompile --backend #{K_BACKEND} oro.k"
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

desc "Build the Oro executable semantic interpreter"
task :build => [KORO_TIMESTAMP, oro_parser_exe]

namespace :test do
  task :one => [:build] do
    require 'highline'  # <- highline gem
    last_pick = Pathname(__FILE__).dirname + ".last_one_test"
    cli = HighLine.new
    test_inputs = FileList['test/**/*.oro'].exclude('test/expected_output/**/*').to_a
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

    expected_outpath = Pathname(__FILE__).dirname + "test" + "expected_output" + (
      Pathname(test_file).relative_path_from Pathname("test")
    )
    Dir.mktmpdir do |dir|
      dir = Pathname(dir)
      actual_outpath = dir + 'actual.txt'
      sh './koro-run', '--output-file', actual_outpath.to_s, test_file
      actual_output = actual_outpath.read
      puts
      puts actual_output
      puts
      if expected_outpath.exist?
        expected_output = expected_outpath.read
        if actual_output == expected_output
          cli.say cli.color("*** Output matched expected ***", :bright_green)
        else
          # TODO: Look up configured diff
          tool = DiffTool.load_from(Pathname(__FILE__).dirname)
          if cli.agree(cli.color("Output does not match expected; #{tool.verb} #{tool.name}? ", :red), true)
            tool.run(expected_outpath, actual_outpath)
          else
            exit 1
          end
        end
      else
        if cli.agree(cli.color("No expected output provided; save this as expected? ", :yellow), true)
          expected_outpath.dirname.mkpath
          expected_outpath.write actual_output
        end
      end
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
