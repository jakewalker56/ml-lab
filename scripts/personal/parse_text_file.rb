#"http://www.amazon.com/s/ref=nb_sb_ss_c_0_20?url=search-alias%3Dstripbooks&field-keywords=money+by+martin+amis&sprefix=Money+by+Martin+Amis%2Caps%2C190"
require 'rubygems'
require 'nokogiri'
require 'open-uri'

if ARGV.count < 2 || ((ARGV.count - 2) % 3 != 0)
  raise "Usage: parse_text_file.rb input_file_name output_file_name [[<start index 1> <end index 1> <variable name 1>]...]"
end

output = File.open(ARGV[1], 'w')

start_index = []
end_index = []
var_name = []
for i in 0...((ARGV.count - 2)/3)
  start_index[i] = ARGV[i * 3 + 2].to_i
  end_index[i] = ARGV[i * 3 + 3].to_i
  var_name[i] = ARGV[i * 3 + 4]
end

output.write(var_name.join(","))
output.write("\n")

File.foreach(ARGV[0]) {|line| 
  out_line = []
  for i in 0...start_index.length
    out_line[i] = line[start_index[i]..end_index[i]]
  end
  output.write(out_line.join(","))
  output.write("\n")
}

output.close
