#!/usr/bin/env ruby


# ##############################################################################

# ##############################################################################
#
# 2022 DAY 2
#
# ##############################################################################

# rock      0 defeats 2   scissors
# paper     1 defeats 0   rock
# scissors  2 defeats 1   paper

# shape   : 1 for Rock, 2 for Paper, and 3 for Scissors
# outcome : 0 if you lost, 3 if the round was a draw, and 6 if you won

# 10718
def d22021()
  input(2202)
    .split("\n").map(&:split)
    .map { | p1, p2 |
      case [p1, p2]
      when ["A", "X"] then 1 + 3 # rock vs rock
      when ["A", "Y"] then 2 + 6 # rock vs papr
      when ["A", "Z"] then 3 + 0 # rock vs scis
      when ["B", "X"] then 1 + 0 # papr vs rock
      when ["B", "Y"] then 2 + 3 # papr vs papr
      when ["B", "Z"] then 3 + 6 # papr vs scis
      when ["C", "X"] then 1 + 6 # scis vs rock
      when ["C", "Y"] then 2 + 0 # scis vs papr
      when ["C", "Z"] then 3 + 3 # scis vs scis
      end
  }.sum
end

# X means you need to lose
# Y means you need to end the round in a draw
# Z means you need to win

# 14652
def d22022()
  input(2202)
    .split("\n").map(&:split)
    .map { | p1, p2 |
      case [p1, p2]
      when ["A", "X"] then 3 + 0 # lose vs rock -> scis
      when ["B", "X"] then 1 + 0 # lose vs papr -> rock
      when ["C", "X"] then 2 + 0 # lose vs scis -> papr
      when ["A", "Y"] then 1 + 3 # draw vs rock -> rock
      when ["B", "Y"] then 2 + 3 # draw vs papr -> papr
      when ["C", "Y"] then 3 + 3 # draw vs scis -> scis
      when ["A", "Z"] then 2 + 6 # win  vs rock -> papr
      when ["B", "Z"] then 3 + 6 # win  vs papr -> scis
      when ["C", "Z"] then 1 + 6 # win  vs scis -> rock
      end
  }.sum
end

# # P1
# i1 = ["A", "B", "C"].index(p1)
# i2 = ["X", "Y", "Z"].index(p2)
# shape_pts = (i2 + 1)
# outcm_pts = (i2 == ((i1 + 1) % 3)) ? 6 : (i2 == i1) ? 3 : 0

# # P2
# i1 = ["A", "B", "C"].index(p1)
# i2 = (p2 == "X") ? ((i1 + 2) % 3) :
#      (p2 == "Z") ? ((i1 + 1) % 3) : i1
# shape_pts = (i2 + 1)
# outcm_pts = (i2 == ((i1 + 1) % 3)) ? 6 : (i2 == i1) ? 3 : 0


# ##############################################################################
#
# 2022 DAY 1
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
