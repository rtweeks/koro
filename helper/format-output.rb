require 'stringio'

class ConfigStyler
  CELL_START = %r{^<[-A-Za-z_]+>\s*}
  START_BLOCK = %r{^[\[(]\s*}
  CELL_END = %r{^</[-A-Za-z_]+>\s*}
  END_BLOCK = %r{^[\])]\s*}
  QSTRING = %r{^"([^\\"]|\\.)*"\s*}

  def self.style(s)
    new(s).process
  end

  def initialize(s)
    @content = s.dup
    @indents = 0
    @out = StringIO.new
    @cur_line = ""
    @map_indents = []
    @map_indent_suspend = 0
  end

  def line_max
    90
  end

  def process
    until @content.empty?
      process_term_and_bound_line
    end
    if @cur_line.strip != ''
      @out << @cur_line << "\n"
      @cur_line = ''
    end
    return @out.string
  end

  private
  def indent
    "  " * @indents
  end

  def process_term_and_bound_line
    cur_line_was = @cur_line
    process_next_term
    if @cur_line.length > line_max
      @out << cur_line_was.rstrip << "\n"
      @cur_line[0, cur_line_was.length] = indent
    end
  end

  def process_next_term
    case @content
    when CELL_START
      cell_tag = consume $&
      new_line! do
        @out << indent << cell_tag.strip << "\n"
        @indents += 1
      end

    when CELL_END
      cell_tag = consume $&
      new_line! do
        @indents -= 1
        @map_indents.clear
        @out << indent << cell_tag.strip << "\n"
      end

    when START_BLOCK
      term = consume $&
      prettify_map
      @cur_line << term
      @indents += 1

    when END_BLOCK
      term = consume $&
      @cur_line << term
      if @map_indents.last == @indents
        @map_indents.pop
      end
      @indents -= 1

    when /^\|->\s*/
      term = consume $&
      if (@map_indents.last || 0) < @indents
        @map_indents.push @indents
      end
      @cur_line << term
      @map_indent_suspend = 1

    when QSTRING, /^\S+\s*/
      term = consume $&
      prettify_map(term)
      @cur_line << term
    end
  end

  def new_line!
    unless @cur_line.strip.empty?
      @out << @cur_line.rstrip << "\n"
    end
    yield if block_given?
    @cur_line.replace(indent)
    return nil
  end

  def consume(s)
    @content[0, s.length] = ''
    return s
  end

  def dump_progress
    puts @out.string
    puts @cur_line unless @cur_line.empty?
  end

  def prettify_map(term = '')
    case
    when %w[yval subroutine].include?(term.strip)
      # do nothing
    when @map_indent_suspend > 0
      @map_indent_suspend -= 1
    when @map_indents.last != @indents
      # do nothing
    else
      new_line!
    end
  end
end
