#!/usr/bin/env ruby

# ##############################################################################

# load "adv21.rb" ; d21011()

# ##############################################################################

# ###########################################################################
#
# 2021 DAY 2
#
# ###########################################################################

# 1714950
def d21021()
  input(2102)
    .split
    .each_slice(2)
    .reduce([0, 0]) {|(hpos, dpth), (dir, n)|
      case dir
      when "forward" then [hpos + n.to_i, dpth]
      when "up"      then [hpos, dpth - n.to_i]
      when "down"    then [hpos, dpth + n.to_i]
      else fail end
    }
    .reduce(&:*)
end

# 1281977850
def d21022()
  input(2102)
    .split
    .each_slice(2)
    .reduce([0, 0, 0]) {|(hpos, dpth, aim), (dir, n)|
      case dir
      when "forward" then [hpos + n.to_i, dpth + n.to_i * aim, aim]
      when "up"      then [hpos, dpth, aim - n.to_i]
      when "down"    then [hpos, dpth, aim + n.to_i]
      else fail end
    }
    .tap { |(hpos, dpth, _)| break hpos * dpth }
end


# ###########################################################################
#
# 2021 DAY 1
#
# ###########################################################################

# 1676
def d21011()
  input(2101)
    .split.map(&:to_i)
    .each_cons(2).count { _1 < _2 }
end

# 1706
def d21012()
  input(2101)
    .split.map(&:to_i)
    .each_cons(3).map(&:sum)
    .each_cons(2).count { _1 < _2 }
end


# ##############################################################################

def input(id)
  IO.read("inputs/#{id}.in")
end

def debug(*args)
  puts "debug: " + args.map(&:to_s).join(" ")
  args.length == 1 ? args.first : args
end
