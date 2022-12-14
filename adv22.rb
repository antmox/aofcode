#!/usr/bin/env ruby

# ##############################################################################

require 'set'

# ##############################################################################
#
# 2022 DAY 14
#
# ##############################################################################

# 498,4 -> 498,6 -> 496,6
# 503,4 -> 502,4 -> 502,9 -> 494,9

# ......+...
# ..........
# ..........
# ..........
# ....#...##
# ....#...#.
# ..###...#.
# ........#.
# ........#.
# #########.

def showscan(scan, sand, src)
  all = (scan + sand + [src])
  (xmin, xmax) = all.map(&:first).minmax
  (ymin, ymax) = all.map(&:last).minmax
  (ymin..ymax).map { |y|
    (xmin..xmax).map { |x|
      scan.include?([x, y]) ? "#" :
      sand.include?([x, y]) ? "o" :
      [x, y] == src         ? "+" : "."
    }.join }.join("\n")
end

def scanrock()
  input(2214.0)
    .split("\n").map{ |line|
      line.scan(/\d+/).map(&:to_i)
      .each_slice(2).each_cons(2).map { |(x1, y1), (x2, y2)|
        (xmin, xmax), (ymin, ymax) = [x1, x2].minmax, [y1, y2].minmax
        if y1 == y2 then
          (xmin..xmax).map { |x| [x, y1] }
        elsif x1 == x2 then
          (ymin..ymax).map { |y| [x1, y] }
        else fail end
    } }.flatten(2).uniq
end

def sandfall(all, ymax, srcd)
  x, y = srcd
  while true
    if y == ymax
      return [x, y] # void/floor reached
    elsif not all.include?([x, y + 1])
      x, y = x, y + 1
    elsif not all.include?([x - 1, y + 1])
      x, y = x - 1, y + 1
    elsif not all.include?([x + 1, y + 1])
      x, y = x + 1, y + 1
    elsif [x, y] == srcd
      return [x, y] # source blocked
    else
      return [x, y] # blocked
    end
  end
end

# .......+...
# ...........
# .......o...
# ......ooo..
# .....#ooo##
# ....o#ooo#.
# ...###ooo#.
# .....oooo#.
# ..o.ooooo#.
# o#########.

# 1330
def d22141()
  scan = scanrock()
  ymax = scan.map(&:last).max
  srcd = [500, 0]

  all = scan.to_set
  while true
    nx, ny = sandfall(all, ymax, srcd)
    all << [nx, ny]
    break if ny == ymax
  end
  # puts showscan(scan, all.to_a - scan, srcd)
  all.length - scan.length - 1
end

# ..........o..........
# .........ooo.........
# ........ooooo........
# .......ooooooo.......
# ......oo#ooo##o......
# .....ooo#ooo#ooo.....
# ....oo###ooo#oooo....
# ...oooo.oooo#ooooo...
# ..oooooooooo#oooooo..
# .ooo#########ooooooo.
# ooooo.......ooooooooo

# 26139
def d22142()
  scan = scanrock()
  ymax = scan.map(&:last).max
  srcd = [500, 0]

  all = scan.to_set
  while true
    nx, ny = sandfall(all, ymax + 1, srcd)
    all << [nx, ny]
    break if [nx, ny] == srcd
  end
  # puts showscan(scan, all.to_a - scan, srcd)
  all.length - scan.length
end


# ##############################################################################
#
# 2022 DAY 13
#
# ##############################################################################

# sorted   -> negative
# equal    -> 0
# unsorted -> positive
def packetscmp(v1, v2)
  case [v1, v2]
  # both values are integer
  in Integer, Integer then v1 - v2
  # exactly one value is an integer
  in Integer, Array   then packetscmp([v1], v2)
  in Array, Integer   then packetscmp(v1, [v2])
  # both values are lists
  in [], []           then  0
  in [], Array        then -1
  in Array, []        then  1
  in Array, Array     then
    (h1, *t1), (h2, *t2) = v1, v2
    cmp = packetscmp(h1, h2)
    cmp != 0 ? cmp : packetscmp(t1, t2)
  end
end

# 5843
def d22131()
  input(2213)
    .split()
    .map { eval(_1) }
    .each_slice(2)
    .map{ packetscmp(_1, _2) }
    .each_with_index.filter_map { |v, i| i + 1 if v < 0 }.sum
end

# 26289
def d22132()
  input(2213)
    .split()
    .map { eval(_1) }
    .then { _1 + [[[2]]] + [[[6]]] }
    .sort { packetscmp(_1, _2) }
    .then { (_1.find_index([[2]]) + 1) * (_1.find_index([[6]]) + 1) }
end


# ##############################################################################
#
# 2022 DAY 12
#
# ##############################################################################

# Sabqponm
# abcryxxl
# accszExk
# acctuvwj
# abdefghi

# v..v<<<<
# >v.vv<<^
# .>vv>E^^
# ..v>>>^^
# ..>>>>>^

def heightmap()
  input(2212)
    .split("\n")
    .each_with_index.map { |l, y|
      l.chars.each_with_index.map { |c, x| [[x, y], c] }
    }.flatten(1).to_h
end

def hike(hmap, starts, end_)
  hval =-> { _1 == "S" ? "a".ord : _1 == "E" ? "z".ord : _1.ord }

  visited, tovisit = [], starts.map { [_1, 0] }

  while not tovisit.empty?
    (x, y), n = tovisit.shift

    return n if [x, y] == end_
    visited.push( [x, y] )

    possible_nexts =
      [ [x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1] ]
        .filter { |ncrd| hmap.key?(ncrd) }
        .filter { |ncrd| hval.(hmap[ncrd]) <= hval.(hmap[[x, y]]) + 1 }
        .filter { |ncrd| not visited.include?(ncrd) }
        .filter { |ncrd| not tovisit.map(&:first).include?(ncrd) }

    tovisit.concat( possible_nexts.map {[_1, n + 1]} )
  end
end

# 481
def d22121()
  hmap = heightmap()
  start, end_ = hmap.key("S"), hmap.key("E")
  hike(hmap, [start], end_)
end

# 480
def d22122()
  hmap = heightmap()
  starts, end_ = hmap.filter { |_, h| h == "a" }.keys, hmap.key("E")
  hike(hmap, starts, end_)
end


# ##############################################################################
#
# 2022 DAY 11
#
# ##############################################################################

# Monkey 0:
#   Starting items: 79, 98
#   Operation: new = old * 19
#   Test: divisible by 23
#     If true: throw to monkey 2
#     If false: throw to monkey 3

def getmonkeys()
  input(2211)
    .split("\n\n")
    .map { |lines|
      (_, *items, _, _, opr, op2, opdiv, dst1, dst0) =
        lines.scan(/[0-9]+|old|[*+=]/)
      { items: items.map(&:to_i), opr: opr, op2: op2,
        opdiv: opdiv.to_i, dst1: dst1.to_i, dst0: dst0.to_i,
        nbins: 0 }
    }
end

def worrylevel(op1, opr, op2)
  op2 = op2 == "old" ? op1 : op2.to_i
  opr == "+" ? op1 + op2 : opr == "*" ? op1 * op2 : fail
end

# 54036
def d22111()
  monkeys = getmonkeys()

  for _ in (1..20) do
    for monkey in monkeys do
      for _ in 1..monkey[:items].length do
        monkey[:nbins] += 1
        item = monkey[:items].shift
        wlvl = worrylevel(item, monkey[:opr], monkey[:op2]) / 3
        dest = (wlvl % monkey[:opdiv] == 0) ? monkey[:dst1] : monkey[:dst0]
        monkeys[dest][:items] << wlvl
      end
    end
  end

  monkeys.map { _1[:nbins] }.sort.reverse.take(2).reduce(&:*)
end

# 13237873355
def d22112()
  monkeys = getmonkeys()

  lcm = monkeys.map { _1[:opdiv] }.reduce(&:lcm)

  for _ in 1..10000 do
    for monkey in monkeys do
      for _ in 1..monkey[:items].length do
        monkey[:nbins] += 1
        item = monkey[:items].shift
        wlvl = worrylevel(item, monkey[:opr], monkey[:op2]) % lcm
        dest = (wlvl % monkey[:opdiv] == 0) ? monkey[:dst1] : monkey[:dst0]
        monkeys[dest][:items] << wlvl
      end
    end
  end

  monkeys.map { _1[:nbins] }.sort.reverse.take(2).reduce(&:*)
end


# ##############################################################################
#
# 2022 DAY 10
#
# ##############################################################################

#           0 |  1
# noop      1 |  1
# addx 3    2 |  1
#           3 |  4
# addx -5   4 |  4
#           5 | -1

def regxs(input)
  input(input)
    .split("\n")
    .reduce([1]) { |values, instr|
      regx = values.last
      case instr
      when /noop/ then
        values << regx
      when /addx ([-0-9]+)/ then
        nregx = regx + $1.to_i
        values << regx << nregx
      end
    }
end

# 13220
def d22101()
  regxs(2210)
    .zip(1..)
    .drop(19)
    .each_slice(40)
    .map(&:first).map{ |v, i| v * i }.sum
end

#   ##..##..##..##..##..##..##..##..##..##..
#   ###...###...###...###...###...###...###.
#   ####....####....####....####....####....
#   #####.....#####.....#####.....#####.....
#   ######......######......######......####
#   #######.......#######.......#######.....

#   ###  #  #  ##  #  # #  # ###  #### #  #
#   #  # #  # #  # # #  #  # #  # #    # #
#   #  # #  # #  # ##   #### ###  ###  ##
#   ###  #  # #### # #  #  # #  # #    # #
#   # #  #  # #  # # #  #  # #  # #    # #
#   #  #  ##  #  # #  # #  # ###  #### #  #

# RUAKHBEK
def d22102()
  regxs(2210)
    .each_slice(40)
    .map(&:each_with_index)
    .map { _1.map { |regx, nbdrwn|
           (regx - nbdrwn).abs <= 1 ? "#" : " " }.join
    }.join("\n").then { puts (_1) }
end


# ##############################################################################
#
# 2022 DAY 9
#
# ##############################################################################

def updatehead(xh, yh, dr)
  case dr
  when "U" then [xh, yh + 1]
  when "D" then [xh, yh - 1]
  when "R" then [xh + 1, yh]
  when "L" then [xh - 1, yh]
  end
end

def updateknot(xh, yh, xt, yt)
  if    xh == xt + 2 and yh == yt     then [xt + 1, yt    ]
  elsif xh == xt - 2 and yh == yt     then [xt - 1, yt    ]

  elsif xh == xt     and yh == yt + 2 then [xt    , yt + 1]
  elsif xh == xt     and yh == yt - 2 then [xt    , yt - 1]

  elsif xh == xt + 2 and yh == yt + 1 then [xt + 1, yt + 1]
  elsif xh >= xt + 1 and yh == yt + 2 then [xt + 1, yt + 1]

  elsif xh == xt - 2 and yh == yt + 1 then [xt - 1, yt + 1]
  elsif xh <= xt - 1 and yh == yt + 2 then [xt - 1, yt + 1]

  elsif xh == xt + 2 and yh == yt - 1 then [xt + 1, yt - 1]
  elsif xh >= xt + 1 and yh == yt - 2 then [xt + 1, yt - 1]

  elsif xh == xt - 2 and yh == yt - 1 then [xt - 1, yt - 1]
  elsif xh <= xt - 1 and yh == yt - 2 then [xt - 1, yt - 1]

  else [xt, yt]
  end
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

# moves.reduce([0, 0, 0, 0, Set[]]) { |(xh, yh, xt, yt, ps), (dr, nb)|
#   nb.to_i.times do
#     xh, yh = updatehead(xh, yh, dr)
#     xt, yt = updateknot(xh, yh, xt, yt)
#     ps << [xt, yt]
#   end; [xh, yh, xt, yt, ps] }.last.length

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
