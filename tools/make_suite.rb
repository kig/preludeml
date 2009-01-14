#!/usr/bin/env ruby
=begin
make_suite.rb - extract oUnit tests from OCaml comments and print them to stdout

Copyright (C) 2007-2008  Ilmari Heikkinen <ilmari.heikkinen@gmail.com>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.


USAGE:

ruby make_suite.rb filename.ml

where filename.ml contains comments like

(**T
  (* this test gets an automatic name of form "test_filename_line_X" *)
  "an expression that evaluates to true" <> ""
  "and another" <> ""
**)

(**T named_test
  foo = foo
**)

(*** named_raw_test_body
  assert_equal "foo" "foo";
  assert_equal "bar" "bar"
**)

=end

if ARGV.size != 1
  puts "Usage: make_suite <file.ml>"
  exit 1
end

file = ARGV[0]

mod = File.basename(file)[/[^.]+/].capitalize

puts <<EOF
open OUnit
open #{mod}

let _iteri f l = ignore (List.fold_left (fun i v -> f v i; i + 1) 0 l)
let _TL = _iteri (fun b i ->
  OUnit.assert_bool ("Line " ^ string_of_int (i+1) ^ " of bool test") b)

EOF

tests = []

data = IO.read(file)
data = data.gsub(/\(\*\*\T(.*?)\*\*\)/m){ |match|
  match[0,4] = "(***"
  lines = match.split(/\n/)
  head = "_TL [ "
  tail = "] "
  lines[1..-1].each{|l|
    l << ";" if !l.strip.empty? and not l.strip =~ /^\(\*.*?\*\)$/
  }
  [ lines[0], head + lines[1..-2].map{|l| l.strip}.join("\n      ") + tail, lines[-1] ].join("\n")
}

auto_name = File.basename(file).downcase.
                 gsub(/^\d+|\.mli?$/,'').
                 gsub(/[^a-z0-9]/, '_')
data_lines = data.split(/\n/)
current_line_num = 0
data.scan(/\(\*\*\*(.*?)\*\*\)/m).each do |a|
  lines = a[0].split(/\n/)
  name, desc = lines.shift.strip.split(/\s+/)
  s = data_lines[current_line_num..-1].join("\n")
  index = s.index(a[0])
  line_num = current_line_num + s[0...index].split(/\n/).size
  current_line_num = line_num + lines.size + 1
  name = "#{name ? name : "test"}_#{auto_name}_line_#{line_num}"
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
