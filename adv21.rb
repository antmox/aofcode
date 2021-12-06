#!/usr/bin/env ruby

# ##############################################################################

# load "adv21.rb" ; d21011()

# ##############################################################################


# ###########################################################################
#
# 2021 DAY 4
#
# ###########################################################################

# 33462
def d21041()
  nums, *brds = input(2104).split("\n\n")
  nums = nums.split(",").map(&:to_i)
  brds = brds.map { _1.split.map(&:to_i) }

  def iswin(brd, nums)
    ( brd.each_slice(5).to_a + # rows
      brd.each_slice(5).to_a.transpose # cols
    ).detect { |values| values.difference(nums).empty? }
  end

  nums.reduce([]) { |acc, num|
    winbrd = brds.filter{ |brd| iswin(brd, acc) }.first
    break winbrd.difference(acc).sum * acc.last if winbrd
    acc + [num]
  }
end

# 30070
def d21042()
  nums, *brds = input(2104).split("\n\n")
  nums = nums.split(",").map(&:to_i)
  brds = brds.map { _1.split.map(&:to_i) }

  def iswin(brd, nums)
    ( brd.each_slice(5).to_a + # rows
      brd.each_slice(5).to_a.transpose # cols
    ).detect { |values| values.difference(nums).empty? }
  end

  nums.reduce([brds, []]) { |(rembrds, acc), num|
    newrems = rembrds.difference(rembrds.filter{ |brd| iswin(brd, acc) })
    break rembrds.last.difference(acc).sum * acc.last if newrems.empty?
    [newrems, acc + [num]]
  }
end


# ###########################################################################
#
# 2021 DAY 3
#
# ###########################################################################

# 1071734
def d21031()
  input(2103)
    .split.map(&:chars)
    .transpose.map(&:tally).map { _1.sort_by(&:last).map(&:first) }
    .transpose.map { _1.join.to_i(2) }.reduce(&:*)
end

# 6124992
def d21032() # so ugly
  inlst = input(2103).split()
  nbits = inlst.first.length
  (0..1).map { |rank|
    (0..nbits).reduce(inlst) { |curlst, bitnum|
      nboccs = {"0" => 0, "1" => 0}.update( curlst.map{ _1[bitnum] }.tally )
      curbit = nboccs.sort_by(&:reverse).at(rank).first
      matches = curlst.select { _1[bitnum] == curbit }
      break matches.first if (matches.count == 1)
      matches
  } }.map { _1.to_i(2) }.reduce(&:*)
end


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
