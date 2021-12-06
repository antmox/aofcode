#!/usr/bin/env ruby

# ##############################################################################

# load "adv21.rb" ; d21011()

# ##############################################################################

# ###########################################################################
#
# 2021 DAY 6
#
# ###########################################################################

# 362639
def d21061()
  def nextgen(g)
    ( g.map{ _1 == 0 ? 6 : _1 - 1 } +  # next timers
      g.filter { _1 == 0 }.map { 8 } ) # new fishes
  end

  initgen = input(2106).split(/[^0-9]/).map(&:to_i)
  (0...80).reduce(initgen) { |acc, _| nextgen(acc) }.length
end

#  1 [124] [ 43] [ 33] [ 55] [ 45] [  0] [  0] [  0] [  0]
#  2 [ 43] [ 33] [ 55] [ 45] [  0] [  0] [124] [  0] [124]
#  3 [ 33] [ 55] [ 45] [  0] [  0] [124] [ 43] [124] [ 43]
#  4 [ 55] [ 45] [  0] [  0] [124] [ 43] [157] [ 43] [ 33]
#  5 [ 45] [  0] [  0] [124] [ 43] [157] [ 98] [ 33] [ 55]
#  6 [  0] [  0] [124] [ 43] [157] [ 98] [ 78] [ 55] [ 45]
#  7 [  0] [124] [ 43] [157] [ 98] [ 78] [ 55] [ 45] [  0]
#  8 [124] [ 43] [157] [ 98] [ 78] [ 55] [ 45] [  0] [  0]
#  9 [ 43] [157] [ 98] [ 78] [ 55] [ 45] [124] [  0] [124]
# 10 [157] [ 98] [ 78] [ 55] [ 45] [124] [ 43] [124] [ 43]
# 11 [ 98] [ 78] [ 55] [ 45] [124] [ 43] [281] [ 43] [157]
# 12 [ 78] [ 55] [ 45] [124] [ 43] [281] [141] [157] [ 98]
# 13 [ 55] [ 45] [124] [ 43] [281] [141] [235] [ 98] [ 78]
# 14 [ 45] [124] [ 43] [281] [141] [235] [153] [ 78] [ 55]
# 15 [124] [ 43] [281] [141] [235] [153] [123] [ 55] [ 45]
# 16 [ 43] [281] [141] [235] [153] [123] [179] [ 45] [124]
# 17 [281] [141] [235] [153] [123] [179] [ 88] [124] [ 43]
# 18 [141] [235] [153] [123] [179] [ 88] [405] [ 43] [281]

# 1639854996917
def d21062()
  def nextgen(g)
    ( # decreased timers
      g.map { |v, n| (v != 0) ? [v - 1, n] : nil }.compact.to_h
    ).merge( # reset timers and new fishes
      g.map { |v, n| (v == 0) ? [[6, n], [8, n]] : nil }.compact.flatten(1).to_h
    ) { |_, old, new| old + new }
  end

  initgen = input(2106).split(/[^0-9]/).map(&:to_i).tally
  (0...256).reduce(initgen) { |acc, _| nextgen(acc) }.values.sum
end

#   # print("\033[A")
#   print((0..8).map {|x| "[%3s] " % g.fetch(x, 0)}.join )


# ###########################################################################
#
# 2021 DAY 5
#
# ###########################################################################

# 6397
def d21051()
  input(2105)
    .split("\n")
    .map { |line| line.scan(/(\d+)/).map{_1.first.to_i} }
    .reduce([]) { |acc, (x1, y1, x2, y2)|
      acc.concat(
        if x1 == x2 then
          (y1..y2).step(y1 <= y2 ? 1 : -1).map { |y| [x1, y] }
        elsif y1 == y2 then
          (x1..x2).step(x1 <= x2 ? 1 : -1).map { |x| [x, y1] }
        else [] end )
    }
    .tally
    .filter { |x, y| y >= 2 }.length
end

# 22335
def d21052()
  input(2105)
    .split("\n")
    .map { |line| line.scan(/(\d+)/).map{_1.first.to_i} }
    .reduce([]) { |acc, (x1, y1, x2, y2)|
      acc.concat(
        if x1 == x2 then
          (y1..y2).step(y1 <= y2 ? 1 : -1).map { |y| [x1, y] }
        elsif y1 == y2 then
          (x1..x2).step(x1 <= x2 ? 1 : -1).map { |x| [x, y1] }
        else
          stepx = (x1 == x2) ? 0 : (x1 <= x2 ? 1 : -1)
          stepy = (y1 == y2) ? 0 : (y1 <= y2 ? 1 : -1)
          (0..((x2 - x1) / stepx)).map { |d| [x1 + d * stepx, y1 + d * stepy] }
        end )
    }
    .tally
    .filter { |x, y| y >= 2 }.length
end


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
