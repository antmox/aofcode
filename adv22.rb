#!/usr/bin/env ruby

# ##############################################################################

require 'set'

# ##############################################################################
#
# 2022 DAY 9
#
# ##############################################################################

def updatehead(xh, yh, dr)
  case dr
  when "U" then yh = yh + 1
  when "D" then yh = yh - 1
  when "R" then xh = xh + 1
  when "L" then xh = xh - 1
  end
  [xh, yh]
end

def updateknot(xh, yh, xt, yt)
  if    yh == yt     and xh - xt ==  2 then xt = xt + 1
  elsif yh == yt     and xh - xt == -2 then xt = xt - 1
  elsif xh == xt     and yh - yt ==  2 then yt = yt + 1
  elsif xh == xt     and yh - yt == -2 then yt = yt - 1

  elsif yh == yt + 1 and xh == xt + 2  then yt = yt + 1; xt = xt + 1
  elsif yh == yt + 1 and xh == xt - 2  then yt = yt + 1; xt = xt - 1
  elsif yh == yt - 1 and xh == xt + 2  then yt = yt - 1; xt = xt + 1
  elsif yh == yt - 1 and xh == xt - 2  then yt = yt - 1; xt = xt - 1

  elsif yh == yt + 2 and xh == xt + 1  then yt = yt + 1; xt = xt + 1
  elsif yh == yt + 2 and xh == xt - 1  then yt = yt + 1; xt = xt - 1
  elsif yh == yt - 2 and xh == xt + 1  then yt = yt - 1; xt = xt + 1
  elsif yh == yt - 2 and xh == xt - 1  then yt = yt - 1; xt = xt - 1

  elsif yh == yt + 2 and xh == xt + 2  then yt = yt + 1; xt = xt + 1
  elsif yh == yt + 2 and xh == xt - 2  then yt = yt + 1; xt = xt - 1
  elsif yh == yt - 2 and xh == xt + 2  then yt = yt - 1; xt = xt + 1
  elsif yh == yt - 2 and xh == xt - 2  then yt = yt - 1; xt = xt - 1
  end
  [xt, yt]
end

# . . # # . .
# . . . # # .
# . # # # # .
# . . . . # .
# s # # # . .
# 13

# 6522
def d22091()
  moves =
    input(2209).split.each_slice(2)

  tail_xys = Set[]
  xh, yh, xt, yt = 0, 0, 0, 0

  for dr, nb in moves
    nb.to_i.times do
      xh, yh = updatehead(xh, yh, dr)
      xt, yt = updateknot(xh, yh, xt, yt)
      tail_xys << [xt, yt]
    end
  end

  tail_xys.length
end

# . . . . . . . . . . . . . . . . . . . . . . . . . .
# . . . . . . . . . . . . . . . . . . . . . . . . . .
# . . . . . . . . . . . . . . . . . . . . . . . . . .
# . . . . . . . . . . . . . . . . . . . . . . . . . .
# . . . . . . . . . . . . . . . . . . . . . . . . . .
# . . . . . . . . . . . . . . . . . . . . . . . . . .
# . . . . . . . . . . . . . . . . . . . . . . . . . .
# . . . . . . . . . . . . . . . . . . . . . . . . . .
# . . . . . . . . . . . . . . . . . . . . . . . . . .
# # . . . . . . . . . . . . . . . . . . . . . . . . .
# # . . . . . . . . . . . . . # # # . . . . . . . . .
# # . . . . . . . . . . . . # . . . # . . . . . . . .
# . # . . . . . . . . . . # . . . . . # . . . . . . .
# . . # . . . . . . . . . . # . . . . . # . . . . . .
# . . . # . . . . . . . . # . . . . . . . # . . . . .
# . . . . # . . . . . . s . . . . . . . . . # . . . .
# . . . . . # . . . . . . . . . . . . . . # . . . . .
# . . . . . . # . . . . . . . . . . . . # . . . . . .
# . . . . . . . # . . . . . . . . . . # . . . . . . .
# . . . . . . . . # . . . . . . . . # . . . . . . . .
# . . . . . . . . . # # # # # # # # . . . . . . . . .
# 36

# 2717
def d22092()
  moves =
    input(2209).split.each_slice(2)

  tail_xys = Set[]
  xyr = (0..9).map{ [0, 0] }

  for dr, nb in moves
    nb.to_i.times do
      xyr[0] = updatehead(*xyr[0], dr)
      for n in (1..9)
        xyr[n] = updateknot(*xyr[n-1], *xyr[n])
      end
      tail_xys << xyr[9]
      end
  end

  tail_xys.length
end


# ##############################################################################
#
# 2022 DAY 8
#
# ##############################################################################

# 3 0 3 7 3      # . . . . .
# 2 5 5 1 2      # . . .   .
# 6 5 3 3 2  ->  # . .   . .  ->  21
# 3 3 5 4 9      # .   .   .
# 3 5 3 9 0      # . . . . .

# 1870
def d22081()
  grid, xmax, ymax =
    input(2208).split("\n").each_with_index.map { |l, y|
      l.chars.each_with_index.map { |c, x| [[x, y], c.to_i]}
    }.flatten(1).to_h.then { [_1] + _1.keys.max }
  #
  grid.keys.map { |x, y|
    # visible from W
    (0..(x-1)).to_a.all?      { |nx| grid[[x, y]] > grid[[nx, y]] } or
    # visible from E
    ((x+1)..(xmax)).to_a.all? { |nx| grid[[x, y]] > grid[[nx, y]] } or
    # visible from N
    (0..(y-1)).to_a.all?      { |ny| grid[[x, y]] > grid[[x, ny]] } or
    # visible from S
    ((y+1)..(ymax)).to_a.all? { |ny| grid[[x, y]] > grid[[x, ny]] }
  }.filter { _1 }.length
end

# 3 0 3 7 3      # . . . . .
# 2 5 5 1 2      # . 1 4 1 .
# 6 5 3 3 2  ->  # . 6 1 2 .  ->  8
# 3 3 5 4 9      # . 1 8 3 .
# 3 5 3 9 0      # . . . . .

# 517440
def d22082()
  grid, xmax, ymax =
    input(2208).split("\n").each_with_index.map { |l, y|
      l.chars.each_with_index.map { |c, x| [[x, y], c.to_i]}
    }.flatten(1).to_h.then { [_1] + _1.keys.max }
  #
  grid.keys.map { |x, y|
    next 0 if (x == 0) or (y == 0) or (x == xmax) or (y == ymax)
    # looking W
    [ (1..(x-1)).to_a.reverse.take_while{|nx| grid[[x, y]] > grid[[nx, y]] },
    # looking E
      ((x+1)..(xmax-1)).to_a.take_while {|nx| grid[[x, y]] > grid[[nx, y]] },
    # looking N
      (1..(y-1)).to_a.reverse.take_while{|ny| grid[[x, y]] > grid[[x, ny]] },
    # looking S
      ((y+1)..(ymax-1)).to_a.take_while {|ny| grid[[x, y]] > grid[[x, ny]] }
    ].map { _1.length.succ }.reduce(&:*)
  }.max
end


# ##############################################################################
#
# 2022 DAY 7
#
# ##############################################################################

# {
#   ["/"]           => 48381165,
#   ["/", "a"]      => 94853,
#   ["/", "a", "e"] => 584,
#   ["/", "d"]      => 24933642
# }

def f2207(input) # { dir => total size }
  input
  .split("\n")
  .each_with_object([[], {}]) { |line, (cwd, dsz)|
    case line
    when /^\$ cd (\S+)/ then
      $1 == "/"  ? (cwd.clear; cwd.push "/") :
      $1 == ".." ? (cwd.pop) : (cwd.push $1)
    when /^(\d+) \S+/
      (0...(cwd.length)).map{ |n|
        dsz[cwd[0..n]] = dsz.fetch(cwd[0..n], 0) + $1.to_i }
    end
  }.then { |_, dsz| dsz }
end

# 1749646
def d22071()
  input(2207)
    .then { f2207(_1) }
    .then { _1.filter{ |_, v| v < 100000 }.values.sum }
end

# 1498966
def d22072()
  input(2207)
    .then { f2207(_1) }
    .then { |dsz|
      needed = 30000000 - (70000000 - dsz[["/"]])
      dsz.values.filter{ _1 >= needed }.sort.first }
end


# ##############################################################################
#
# 2022 DAY 6
#
# ##############################################################################

# "mjqjpqmgbljsphdztnvjfqwrcgsmlb"    # 7  19
# "bvwbjplbgvbhsrlpgdmjqwftvncz"      # 5  23
# "nppdvjthqldpwncqszvftbrmjlhg"      # 6  23
# "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" # 10 29
# "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"  # 11 26

def f2206(input, size)
  input
    .chars
    .each_cons(size)
    .to_a
    .find_index{ |l| l.uniq.length == size }
    .then{ _1 + size }
end

def d22060()
  <<~END
    mjqjpqmgbljsphdztnvjfqwrcgsmlb
    bvwbjplbgvbhsrlpgdmjqwftvncz
    nppdvjthqldpwncqszvftbrmjlhg
    nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
    zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
  END
  .split.map{ |line|
    [ f2206(line, 4), f2206(line, 14) ]
  }
end

# 1647
def d22061()
  input(2206)
    .then{ f2206(_1, 4) }
end

# 2447
def d22062()
  input(2206)
    .then{ f2206(_1, 14) }
end


# ##############################################################################
#
# 2022 DAY 5
#
# ##############################################################################

#     [D]
# [N] [C]
# [Z] [M] [P]
#  1   2   3
#
# move 1 from 2 to 1
# move 3 from 1 to 3
# move 2 from 2 to 1
# move 1 from 1 to 2

module D2205
  def inputd()
    (stacks, instrs) =
      input(2205).split("\n\n")
    stacks =
      stacks.split("\n").map{ |ln|
        ln.chars.each_slice(4).map{ |s| s.at(1) }
      }.transpose.map { |l| l.join.scan(/[A-Z]/) }
    instrs =
      instrs.split("\n").map{ |ln|
        ln.scan(/\d+/).map(&:to_i) }
    [ stacks, instrs ]
  end
end

#                #         [Z]    #         [Z]    #         [Z]
# [D]            #         [N]    #         [N]    #         [N]
# [N] [C]        #     [C] [D]    # [M]     [D]    #         [D]
# [Z] [M] [P]    #     [M] [P]    # [C]     [P]    # [C] [M] [P]
#  1   2   3     #  1   2   3     #  1   2   3     #  1   2   3

# QNHWJVJZW
def d22051()
  include D2205
  inputd().then{ |stacks, instrs|
    instrs.inject(stacks) { |stacks, (nb, from, to)|
      stacks[to - 1].unshift(*stacks[from - 1].shift(nb).reverse)
      stacks
    }.map(&:first).join
  }
end

#                #         [D]    #         [Z]    #         [Z]
# [D]            #         [N]    #         [N]    #         [N]
# [N] [C]        #     [C] [Z]    # [C]     [D]    #         [D]
# [Z] [M] [P]    #     [M] [P]    # [M]     [P]    # [M] [C] [P]
#  1   2   3     #  1   2   3     #  1   2   3     #  1   2   3

# BPCZJLFJW
def d22052()
  include D2205
  inputd().then{ |stacks, instrs|
    instrs.inject(stacks) { |stacks, (nb, from, to)|
      stacks[to - 1].unshift(*stacks[from - 1].shift(nb))
      stacks
    }.map(&:first).join
  }
end


# ##############################################################################
#
# 2022 DAY 4
#
# ##############################################################################

# 475
def d22041()
  input(2204)
    .split
    .filter { |line|
      p11, p12, p21, p22 = line.split(/[^0-9]/).map(&:to_i)
      (p11 >= p21 && p12 <= p22) || (p21 >= p11 && p22 <= p12) }
    .length
end

# 825
def d22041()
  input(2204)
    .split
    .filter { |line|
      p11, p12, p21, p22 = line.split(/[^0-9]/).map(&:to_i)
      p11.between?(p21, p22) || p12.between?(p21, p22) ||
        p21.between?(p11, p12) || p22.between?(p11, p12) }
    .length
end


# ##############################################################################
#
# 2022 DAY 3
#
# ##############################################################################

# 8394
def d22031()
  input(2203)
    .split
    .map(&:chars)
    .map{ |l|
      l.each_slice(l.size / 2).to_a.then { |l1, l2| l1.intersection(l2) } }
    .flatten
    .map { |x|
      x.between?("a", "z") ? (x.ord - "a".ord + 1) :
      x.between?("A", "Z") ? (x.ord - "A".ord + 27) : fail
    }.sum
end

# 2413
def d22032()
  input(2203)
    .split
    .map(&:chars)
    .each_slice(3)
    .map{ |l1, l2, l3| l1.intersection(l2, l3).first }
    .map { |x|
      x.between?("a", "z") ? (x.ord - "a".ord + 1) :
      x.between?("A", "Z") ? (x.ord - "A".ord + 27) : fail
    }.sum
end


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
