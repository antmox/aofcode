#!/usr/bin/env ruby


# ##############################################################################

# ##############################################################################
#
# 2021 DAY 1
#
# ##############################################################################

# 72240
def d22011()
  input(2201)
    .split("\n\n")
    .map { _1.split.map(&:to_i).sum }
    .max
end

# 210957
def d22012()
  input(2201)
    .split("\n\n")
    .map { _1.split.map(&:to_i).sum }
    .sort.reverse
    .take(3)
    .sum
end


# ##############################################################################

def input(id)
  IO.read("#{File.dirname(__FILE__)}/inputs/#{id}.in")
end

def debug(*args)
  puts "debug: " + args.map(&:to_s).join(" ")
  args.length == 1 ? args.first : args
end


# ##############################################################################

fail if RUBY_VERSION.to_f < 3.0

if $PROGRAM_NAME == __FILE__

  proc, *args = ARGV

  proc ||= private_methods.filter {_1.to_s.match?(/d\d{5}/) }.sort.last

  debug("call #{proc} ( #{args} )")

  fstr = Time.now
  p self.send(proc.to_sym, *args)
  fend = Time.now

  debug("took #{fend - fstr}s")

end
