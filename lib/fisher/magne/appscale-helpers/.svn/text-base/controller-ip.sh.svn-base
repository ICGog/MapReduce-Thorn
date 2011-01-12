#!/usr/bin/ruby
require "yaml"
path = ""
if ARGV[0]
  path = "#{ARGV[0]}/"
end
tree = YAML::load_file( "#{path}ip.yaml" ) 
puts tree[:controller]
