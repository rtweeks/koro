require 'pathname'

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
