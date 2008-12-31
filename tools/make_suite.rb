#!/usr/bin/env ruby

if ARGV.size != 1 
  puts "Usage: make_suite <file.ml>"
  exit 1
end

file = ARGV[0]

mod = File.basename(file)[/[^.]+/].capitalize

puts <<EOF
open OUnit
open #{mod}

EOF

tests = []

data = IO.read(file)
data = data.gsub(/\(\*\*\T(.*?)\*\*\)/m){ |match|
  match[0,4] = "(***"
  lines = match.split(/\n/)
  head = "List.iter (OUnit.assert_equal ~printer:string_of_bool true) ["
  tail = "]"
  lines[1..-1].each{|l| 
    l << ";" if !l.strip.empty? and not l.strip =~ /^\(\*.*?\*\)$/
  }
  [ lines[0], head + lines[1..-2].join("\n") + tail, lines[-1] ].join("\n")
}

auto_name = "test_" + File.basename(file).downcase.
                           gsub(/^\d+|\.mli?$/,'').
                           gsub(/[^a-z0-9]/, '_')
data_lines = data.split(/\n/)
current_line_num = 0
data.scan(/\(\*\*\*(.*?)\*\*\)/m).each do |a|
  lines = a[0].split(/\n/)
  name, desc = lines.shift.strip.split(/\s+/)
  if !name || name.empty?
    s = data_lines[current_line_num..-1].join("\n")
    index = s.index(a[0])
    line_num = current_line_num + s[0...index].split(/\n/).size
    current_line_num = line_num + lines.size + 1
    name = "#{auto_name}_line_#{line_num}"
  end
  desc ||= name
  tests << [name, desc]

  puts <<-EOF
let #{name} () =
#{lines.join("\n")}

  EOF
end

tests = tests.map{|name, desc| "    \"#{desc}\" >:: #{name}"}.join(";\n")

puts <<EOF

let suite = "#{mod} unit tests" >:::
  [
#{tests}
  ]

let () = Tests.register suite
EOF
