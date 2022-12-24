#!/usr/bin/env ruby

# ##############################################################################

require 'set'

# ##############################################################################
#
# 2022 DAY 24
#
# ##############################################################################

#  #.######
#  #>>.<^<#
#  #.<..<<#
#  #>v.><>#
#  #<^v^^>#
#  ######.#

def getblizzs()
  blizz =
    input(2224).split("\n").each_with_index.map { |l, y|
      l.chars.each_with_index.filter_map { |c, x|
        case c
        when ">" then [x - 1, y - 1, 1,  0]
        when "<" then [x - 1, y - 1, -1, 0]
        when "^" then [x - 1, y - 1, 0, -1]
        when "v" then [x - 1, y - 1, 0,  1]
        else nil end
    } }.flatten(1)
  xmax, ymax = blizz.map { |x, y, _, _| [x, y] }.transpose.map(&:max)
  blizy = blizz.filter{ |x, y, dx, dy| dy == 0 }.group_by { |_, y, _, _| y }
  blizx = blizz.filter{ |x, y, dx, dy| dx == 0 }.group_by { |x, _, _, _| x }
  [blizx, blizy, xmax, ymax]
end

def expmoves(blizx, blizy, xmax, ymax, x, y, t)
  res = []
  for nx, ny in [
    [x, y - 1], [x - 1, y], [x, y], [x + 1, y], [x, y + 1] ] do
    # valid position ?
    next if (
        (x < 0) or (x > xmax) or (y < -1) or (y == -1 and x != 0) or
        (y > (ymax + 1)) or (y == (ymax + 1) and x != xmax) )
    # vertical bliz here ?
    next if blizx.fetch(nx, []).any? { |wx, wy, wdx, wdy|
      px = (wx + wdx * (t + 1)) % (xmax + 1)
      py = (wy + wdy * (t + 1)) % (ymax + 1)
      ( px == nx and py == ny ) }
    # horizontal bliz here ?
    next if blizy.fetch(ny, []).any? { |wx, wy, wdx, wdy|
      px = (wx + wdx * (t + 1)) % (xmax + 1)
      py = (wy + wdy * (t + 1)) % (ymax + 1)
      ( px == nx and py == ny ) }
    #
    res << [nx, ny, t + 1]
  end
  res
end

def blizzastar(blizx, blizy, xmax, ymax, start, goal, t0)
  visited = Set[]
  tovisit = [ [*start, t0] ] # [x, y, t]
  while not tovisit.empty? do
    tovisit.sort_by! { |x, y, t| t + (goal[0] - x).abs + (goal[1] - y).abs }
    x, y, t = tovisit.shift
    visited.add([x, y, t])
    return t if [x, y] == goal
    mvs = expmoves(blizx, blizy, xmax, ymax, x, y, t)
    mvs = mvs.filter { |mv| not tovisit.include?(mv) }
    mvs = mvs.filter { |mv| not visited.include?(mv) }
    tovisit.push(*mvs)
  end
end

# 269
def d22241()
  blizx, blizy, xmax, ymax = getblizzs()
  start, goal = [0, -1], [xmax, ymax + 1]
  blizzastar(blizx, blizy, xmax, ymax, start, goal, 0)
end

# 825 (92s)
def d22242()
  blizx, blizy, xmax, ymax = getblizzs()
  start, goal = [0, -1], [xmax, ymax + 1]
  t1 = debug(blizzastar(blizx, blizy, xmax, ymax, start, goal, 0))
  t2 = debug(blizzastar(blizx, blizy, xmax, ymax, goal, start, t1))
  debug(blizzastar(blizx, blizy, xmax, ymax, start, goal, t2))
end


# ##############################################################################
#
# 2022 DAY 23
#
# ##############################################################################

#  .....  #  ..##.  #  .....  #  ..#..
#  ..##.  #  .....  #  ..##.  #  ....#
#  ..#..  #  ..#..  #  .#...  #  #....
#  .....  #  ...#.  #  ....#  #  ....#
#  ..##.  #  ..#..  #  .....  #  .....
#  .....  #  .....  #  ..#..  #  ..#..

def getelves()
  input(2223).split("\n").each_with_index.map { |l, y|
    l.chars.each_with_index.filter_map { |c, x| [x, y] if c == "#" }
  }.flatten(1).to_set
end

def checkdir(elves, dir, x, y)
  case dir
  when "N" then
    return [x, y - 1] if not Set[[x, y - 1], [x - 1, y - 1], [x + 1, y - 1]].intersect?(elves)
  when "S" then
    return [x, y + 1] if not Set[[x, y + 1], [x - 1, y + 1], [x + 1, y + 1]].intersect?(elves)
  when "W" then
    return [x - 1, y] if not Set[[x - 1, y], [x - 1, y + 1], [x - 1, y - 1]].intersect?(elves)
  when "E" then
    return [x + 1, y] if not Set[[x + 1, y], [x + 1, y + 1], [x + 1, y - 1]].intersect?(elves)
  else fail end
  return nil
end

def checkadj(elves, x, y)
  Set[[x - 1, y - 1], [x    , y - 1], [x + 1, y - 1],
      [x - 1, y    ],                 [x + 1, y    ],
      [x - 1, y + 1], [x    , y + 1], [x + 1, y + 1]
     ].intersect?(elves)
end

def elvesround(elves, n)
  dirs = ["N", "S", "W", "E"].rotate(n - 1)
  # first half -> propositions
  props = {}
  for x, y in elves do
    next if not checkadj(elves, x, y)
    for d in dirs do
      pr = checkdir(elves, d, x, y)
      next if not pr
      (props[pr] ||= []) << [x, y]
      break
    end
  end
  # second half -> move
  for k, v in props.filter { |_, l| l.length == 1 } do
    elves.delete(v.first)
    elves.add(k)
  end
  #
  elves
end

# 4236
def d22231()
  elves = getelves()

  for n in (1..10) do
    elves = elvesround(elves, n)
  end

  (xmin, xmax), (ymin, ymax) = elves.to_a.transpose.map(&:minmax)
  (xmax + 1 - xmin) * (ymax + 1 - ymin) - elves.length
end

# 1023 (55s)
def d22232()
  elves = getelves()

  for n in (1..) do
    debug(n) if (n % 100) == 0
    elve2 = elves.clone
    elves = elvesround(elves, n)
    break n if elves == elve2
  end
end


# ##############################################################################
#
# 2022 DAY 22
#
# ##############################################################################

def nextpos(board, xmax, ymax, x, y, f)
  # move
  nx, ny = [x + 1, y] if f == "E"
  nx, ny = [x, y + 1] if f == "S"
  nx, ny = [x - 1, y] if f == "W"
  nx, ny = [x, y - 1] if f == "N"
  # off-map -> wrap around
  if not board[[nx, ny]] then
    case f
    when "E" then
      nx = x.downto(-1).map   { |xx| break (xx + 1) if not board[[xx, ny]] }
    when "W" then
      nx = x.upto(xmax+1).map { |xx| break (xx - 1) if not board[[xx, ny]] }
    when "S" then
      ny = y.downto(-1).map   { |yy| break (yy + 1) if not board[[nx, yy]] }
    when "N" then
      ny = y.upto(ymax+1).map { |yy| break (yy - 1) if not board[[nx, yy]] }
    else fail end
  end
  # wall -> stop
  return [x, y] if board[[nx, ny]] == "#"
  [nx, ny]
end

# 164014
def d22221()
  board, path = input(2222).split("\n\n")
  board = board.split("\n").each_with_index.map { |l, y|
    l.chars.each_with_index.filter_map { |c, x| [[x + 1, y + 1], c] if c != " " }
  }.flatten(1).to_h.then { _1.default = nil; _1 }
  path = path.scan(/\d+|[A-Z]/)
  xmax, ymax = board.keys.transpose.map(&:max)
  dirs = ['E', 'S', 'W', 'N']
  x, y, f = (0..).find { |x| board[[x, 1]] == '.' }, 1, 'E'

  for i in path
    if i == "R" then
      f = dirs[(dirs.find_index(f) + 1) % 4]
    elsif i == "L" then
      f = dirs[(dirs.find_index(f) - 1) % 4]
    else
      for i in (1..i.to_i).to_a do
        x, y = nextpos(board, xmax, ymax, x, y, f)
      end
    end
  end

  [4 * x, 1000 * y, dirs.find_index(f)].sum
end

#  _  2  3
#  _  5  _
#  7  8  _
# 10  _  _

def nextposp2(board, xmax, ymax, x, y, f)
  # move
  nx, ny, nf = [x + 1, y, f] if f == "E"
  nx, ny, nf = [x, y + 1, f] if f == "S"
  nx, ny, nf = [x - 1, y, f] if f == "W"
  nx, ny, nf = [x, y - 1, f] if f == "N"
  # off-map -> find new pos on cube and new facing
  if not board[[nx, ny]] then
    if false then
      nil
    elsif f == "N" and y == 1 and x >= 1 and x <= 100 then
      nx, ny, nf = 1, 100 + nx, "E"
    elsif f == "W" and x == 1 and y >= 151 and y <= 200 then
      nx, ny, nf = ny - 100, 1, "S"
    elsif f == "W" and x == 1 and y >= 101 and y <= 150 then
      nx, ny, nf = 51, 151 - ny, "E"
    elsif f == "W" and x == 51 and y >= 1 and y <= 50 then
      nx, ny, nf = 1, 151 - ny, "E"
    elsif f == "N" and y == 1 and x >= 101 and x <= 150 then
      nx, ny, nf = nx - 100, 200, "N"
    elsif f == "S" and y == 200 and x >= 1 and x <= 50 then
      nx, ny, nf = nx + 100, 1, "S"
    elsif f == "E" and x == 100 and y >= 51 and y <= 100 then
      nx, ny, nf = 50 + ny, 50, "N"
    elsif f == "N" and y == 101 and x >= 1 and x <= 50 then
      nx, ny, nf = 51, 50 + nx, "E"
    elsif f == "W" and x == 51 and y >= 51 and y <= 100 then
      nx, ny, nf = ny - 50, 101, "S"
    elsif f == "E" and x == 150 and y >= 1 and y <= 50 then
      nx, ny, nf = 100, 151 - ny, "W"
    elsif f == "S" and y == 50 and x >= 101 and x <= 150 then
      nx, ny, nf = 100, nx - 50, "W"
    elsif f == "E" and x == 100 and y >= 101 and y <= 150 then
      nx, ny, nf = 150, 151 - ny, "W"
    elsif f == "S" and y == 150 and x >= 51 and x <= 100 then
      nx, ny, nf = 50, 100 + nx, "W"
    elsif f == "E" and x == 50 and y >= 151 and y <= 200 then
      nx, ny, nf = ny - 100, 150, "N"
    # XXXX other transitions not needed
    else debug("XXX", [x, y, f]);  fail; end
  end
  # wall -> stop
  return [x, y, f] if board[[nx, ny]] == "#"
  [nx, ny, nf]
end

# 135059 -> too high
# 183022 -> too high

# 47525
def d22222()
  board, path = input(2222).split("\n\n")
  board = board.split("\n").each_with_index.map { |l, y|
    l.chars.each_with_index.filter_map { |c, x| [[x + 1, y + 1], c] if c != " " }
  }.flatten(1).to_h.then { _1.default = nil; _1 }
  path = path.scan(/\d+|[A-Z]/)
  xmax, ymax = board.keys.transpose.map(&:max)
  dirs = ['E', 'S', 'W', 'N']
  x, y, f = (0..).find { |x| board[[x, 1]] == '.' }, 1, 'E'

  for i in path
    if i == "R" then
      f = dirs[(dirs.find_index(f) + 1) % 4]
    elsif i == "L" then
      f = dirs[(dirs.find_index(f) - 1) % 4]
    else
      for n in (1..i.to_i).to_a do
        x, y, f = nextposp2(board, xmax, ymax, x, y, f)
      end
    end
  end
  debug(x, y, f)
  [4 * x, 1000 * y, dirs.find_index(f)].sum
end


# ##############################################################################
#
# 2022 DAY 21
#
# ##############################################################################

# root: pppw + sjmn
# dbpl: 5
# cczh: sllz + lgvd
# zczc: 2
# ptdq: humn - dvpt
# dvpt: 3
# lfqf: 4
# humn: 5
# ljgn: 2
# sjmn: drzm * dbpl
# sllz: 4
# pppw: cczh / lfqf
# lgvd: ljgn * ptdq
# drzm: hmdt - zczc
# hmdt: 32

def getmonkeyops()
  input(2221).split("\n")
    .map { |line| mk, *mo = line.split(/[ :]+/); [mk, mo] }.to_h
end

def evalmonkey(monkeys, monkey)
  case monkeys[monkey]
    in [x]         then x.to_i
    in [x, "+", y] then evalmonkey(monkeys, x) + evalmonkey(monkeys, y)
    in [x, "-", y] then evalmonkey(monkeys, x) - evalmonkey(monkeys, y)
    in [x, "*", y] then evalmonkey(monkeys, x) * evalmonkey(monkeys, y)
    in [x, "/", y] then evalmonkey(monkeys, x) / evalmonkey(monkeys, y)
  else fail end
rescue
  nil
end

# 85616733059734
def d22211()
  monkeys = getmonkeyops()
  evalmonkey(monkeys, "root")
end

# 3560324848168
def d22212()
  monkeys = getmonkeyops()
  monkeys["humn"] = nil # will fail eval
  monkeys["root"][1] = "="

  tovisit, expectd = "root", 0
  while true do
    debug("TOVISIT", tovisit, monkeys[tovisit], expectd)
    break expectd if tovisit == "humn"

    r1, op, r2 = monkeys[tovisit]
    v1, v2 = evalmonkey(monkeys, r1), evalmonkey(monkeys, r2)
    fail unless v1 or v2

    tovisit = v1 ? r2 : r1
    case [v1 , op , v2 ]
    in   [nil, "+", v2 ] then expectd = expectd - v2 # ? + v2 == X  ->  X - v2
    in   [v1 , "+", nil] then expectd = expectd - v1 # v1 + ? == X  ->  X - v1
    in   [nil, "-", v2 ] then expectd = expectd + v2 # ? - v2 == X  ->  X + v2
    in   [v1 , "-", nil] then expectd = v1 - expectd # v1 - ? == X  ->  v1 - X
    in   [nil, "/", v2 ] then expectd = expectd * v2 # ? / v2 == X  ->  X * v2
    in   [v1 , "/", nil] then expectd = v1 / expectd # v1 / ? == X  ->  v1 / X
    in   [nil, "*", v2 ] then expectd = expectd / v2 # ? * v2 == X  ->  X / v2
    in   [v1 , "*", nil] then expectd = expectd / v1 # v1 * ? == X  ->  X / v1
    in   [v1,  "=", v2 ] then expectd = (v1 or v2)   # initial root case
    else fail end
  end
end


# ##############################################################################
#
# 2022 DAY 20
#
# ##############################################################################

# 5962
def d22201()
  list = input(2220).split.map(&:to_i)
  indx = (0...list.length).to_a
  for x, n in list.each_with_index do
    i = indx.find_index(n)
    indx.rotate!(i)
    z = indx.shift
    indx.rotate!(x)
    indx.unshift(z)
  end
  list = indx.map {|i| list[i] }
  list.rotate!(list.find_index(0))
  [1000, 2000, 3000].map { |n| list.rotate(n).first }.sum
end

# 9862431387256
def d22202()
  list = input(2220).split.map(&:to_i).map { _1 * 811589153 }
  indx = (0...list.length).to_a
  10.times do
    for x, n in list.each_with_index do
      i = indx.find_index(n)
      indx.rotate!(i)
      z = indx.shift
      indx.rotate!(x)
      indx.unshift(z)
    end
  end
  list = indx.map {|i| list[i] }
  list.rotate!(list.find_index(0))
  [1000, 2000, 3000].map { |n| list.rotate(n).first }.sum
end


# ##############################################################################
#
# 2022 DAY 19
#
# ##############################################################################

# Blueprint 1:
#   Each ore robot costs 4 ore.
#   Each clay robot costs 2 ore.
#   Each obsidian robot costs 3 ore and 14 clay.
#   Each geode robot costs 2 ore and 7 obsidian.

# Blueprint 2:
#   Each ore robot costs 2 ore.
#   Each clay robot costs 3 ore.
#   Each obsidian robot costs 3 ore and 8 clay.
#   Each geode robot costs 3 ore and 12 obsidian.

# [ [1, 4, 2, 3, 14, 2, 7], [2, 2, 3, 3, 8, 3, 12] ]

def geobreak(bp, timeleft)

  xore, yore, zore, zcla, wore, wobs = bp
  more = [xore, yore, zore, wore].max

  visited, best = Set[], 0
  tovisit = [[
    0, 0, 0, 0, # nore, ncla, nobs, ngeo
    1, 0, 0, 0, # rore, rcla, robs, rgeo
    timeleft    # time
  ]]

  while not tovisit.empty? do

    current = tovisit.shift

    nore, ncla, nobs, ngeo, rore, rcla, robs, rgeo, time = current

    best = ngeo if ngeo > best
    next if time == 0

    # ############################################
    # speed up exploration by simplifying similar values
    # XXX there is surely something way better to do here :-(
    # XXX   keep best (ngeo) tovisits states for each time val ?
    # XXX   simplif before adding to tovisit stack ?
    # XXX   do not create robot if not enough time for next elt ?
    #  -> do not generate more elemts than 2 * max consumption ?
    nobs = [nobs, wobs * 2].min
    ncla = [ncla, zcla * 2].min
    nore = [nore, more * 2].min
    #  -> do not generate more robots than max consumption
    robs = [robs, wobs].min
    rcla = [rcla, zcla].min
    rore = [rore, more].min
    #
    current = nore, ncla, nobs, ngeo, rore, rcla, robs, rgeo, time
    # ############################################

    next if visited.include?(current)
    visited << current

    # create rgeo
    if nore >= wore and nobs >= wobs then
      tovisit << [
        nore + rore - wore, ncla + rcla, nobs + robs - wobs, ngeo + rgeo,
        rore, rcla, robs, rgeo + 1, time - 1 ]
      next # create rgeo is always the best choice
    end

    # create robs
    if nore >= zore and ncla >= zcla then
      tovisit << [
        nore + rore - zore, ncla + rcla - zcla, nobs + robs, ngeo + rgeo,
        rore, rcla, robs + 1, rgeo, time - 1 ]
    end

    # create rcla
    if nore >= yore then
      tovisit << [
        nore + rore - yore, ncla + rcla, nobs + robs, ngeo + rgeo,
        rore, rcla + 1, robs, rgeo, time - 1 ]
    end

    # create rore
    if nore >= xore then
      tovisit << [
        nore + rore - xore, ncla + rcla, nobs + robs, ngeo + rgeo,
        rore + 1, rcla, robs, rgeo, time - 1 ]
    end

    # create nothing
    tovisit << [
      nore + rore, ncla + rcla, nobs + robs, ngeo + rgeo,
      rore, rcla, robs, rgeo, time - 1 ]
  end

  best
end

# debug: 1 9 9
# debug: 2 0 0
# debug: 3 1 3
# debug: 4 9 36
# debug: 5 0 0
# debug: 6 2 12
# debug: 7 8 56
# debug: 8 1 8
# debug: 9 0 0
# debug: 10 0 0
# debug: 11 6 66
# debug: 12 2 24
# debug: 13 2 26
# debug: 14 0 0
# debug: 15 2 30
# debug: 16 0 0
# debug: 17 0 0
# debug: 18 13 234
# debug: 19 0 0
# debug: 20 7 140
# debug: 21 6 126
# debug: 22 0 0
# debug: 23 15 345
# debug: 24 1 24
# debug: 25 3 75
# debug: 26 0 0
# debug: 27 4 108
# debug: 28 13 364
# debug: 29 6 174
# debug: 30 10 300

# 2160 (20s)
def d22191()
  input(2219)
    .split("\n")
    .map { |l| l.scan(/\d+/).map(&:to_i) }
    .map { |(nb, *bp)| x = geobreak(bp, 24); debug(nb, x, nb * x); nb * x }
    .sum
end

# debug: 58
# debug: 10
# debug: 23

# 13340 (25s)
def d22192()
  input(2219)
    .split("\n")
    .take(3)
    .map { |l| l.scan(/\d+/).map(&:to_i) }
    .map { |(nb, *bp)| x = geobreak(bp, 32); debug(x); x }
    .reduce(&:*)
end


# ##############################################################################
#
# 2022 DAY 18
#
# ##############################################################################

def neighbors((x, y, z))
  [ [x - 1, y, z], [x + 1, y, z],
    [x, y - 1, z], [x, y + 1, z],
    [x, y, z - 1], [x, y, z + 1] ]
end

# 4450
def d22181()
  cubes =
    input(2218).scan(/[0-9]+/).map(&:to_i).each_slice(3).to_set
  cubes.map { |cube|
    6 - neighbors(cube).filter{ |n| cubes.include?(n) }.length
  }.sum
end

# 2564
def d22182()
  cubes =
    input(2218).scan(/[0-9]+/).map(&:to_i).each_slice(3).to_set

  # list non-trapped neighbors
  dmax = cubes.to_a.flatten.max + 1
  reachbs, tovisit = Set[], [[dmax, dmax, dmax]]
  while not tovisit.empty?
    cube = tovisit.shift
    reachbs.add(cube)
    tovisit.push(
      *neighbors(cube)
         .filter { |ngh| ngh.all? { |c| (-1..dmax).cover?(c) } }
         .filter { |ngh| not cubes.include?(ngh) }
         .filter { |ngh| not tovisit.include?(ngh) }
         .filter { |ngh| not reachbs.include?(ngh) } )
  end
  #

  cubes.map { |cube|
    6 - neighbors(cube).filter{ |n|
      cubes.include?(n) or not reachbs.include?(n) }.length
  }.sum
end


# ##############################################################################
#
# 2022 DAY 17
#
# ##############################################################################

#
#  ....  ....  ..#.  #...  ....
#  ....  .#..  ..#.  #...  ....
#  ....  ###.  ..#.  #...  ##..
#  ####  .#..  ###.  #...  ##..
#
#  0     1     2     3     4

@shapes = [
  [ [0, 0], [1, 0], [2, 0], [3, 0]         ],
  [ [1, 0], [0, 1], [1, 1], [2, 1], [1, 2] ],
  [ [0, 0], [1, 0], [2, 0], [2, 1], [2, 2] ],
  [ [0, 0], [0, 1], [0, 2], [0, 3]         ],
  [ [0, 0], [1, 0], [0, 1], [1, 1]         ] ]

# |....#..|
# |....#..|
# |....#..|
# |@@.###.|
# |@@.####|
# |.###.#.|
# |..####.|
# |..#....|
# |..#...#|
# |..#...#|
# |..#####|
# |#####..|
# |##.#...|
# |.####..|
# +-------+

def showtower(tower, shape)
  ymax = (tower | shape.to_set).map(&:last).max
  towr = ymax.downto(-1).map { |y|
    (-1).upto(7).map { |x|
      (((x == -1) or (x == 7)) and (y == -1)) ? "+"
      : ((x == -1) or (x == 7))               ? "|"
      : (y == -1)                             ? "-"
      : (tower.include?([x, y]))              ? "#"
      : (shape.include?([x, y]))              ? "@"
      : "." }.join
  }.join("\n")
  puts towr
end

# jets, tower, tmax, nbrnd, nbshp -> newtower, newtmax, newnbrnd
def fallrock(jets, tower, tmax, nrnd, nshp)
  shpn = nshp % @shapes.length       # shape type number
  newr = @shapes[shpn].map { |x, y|  # coords of new rock
    [x + 2, y + tmax + 4] }

  while true do
    # first, apply horizontal jet move if possible
    jetn = jets[nrnd % jets.length]  # jet offset
    nrnd += 1

    new2 = newr.map { |x, y| [x + jetn, y] }
    possible = new2.all? { |x, y|
      (x >= 0) and (x <= 6) and not tower.include?([x, y]) }
    newr = new2 if possible

    # then, fall down one level if possible
    new3 = newr.map { |x, y| [x, y - 1] }
    possible = new3.all? { |x, y|
      (y >= 0) and not tower.include?([x, y]) }
    newr = new3 if possible

    break if not possible
  end

  tower |= newr.to_set
  tmax = tower.map(&:last).max
  [tower, tmax, nrnd]
end

# 3133
def d22171()
  jets =
    input(2217).chars.map{ _1 == ">" ? 1 : -1 }

  towr, tmax, nrnd, nshp = Set[], -1, 0, 0
  while true do
    towr, tmax, nrnd = fallrock(jets, towr, tmax, nrnd, nshp)
    nshp += 1
    break if nshp == 2022
  end
  tmax + 1
end

# wrong frontier but it probably does not matter
# -> it should work anyway if there is at least one cycle on one of these
# convex frontiers
def getfrontier(tower, tmax)
  tower
    .group_by { |x, y| x }
    .map { |x, cells| [x, tmax - cells.map(&:last).max] }
    .sort.map(&:last)
end

# 1547953216393
def d22172()
  jets =
    input(2217).chars.map{ _1 == ">" ? 1 : -1 }
  maxshpn = 1000000000000
  cache = {} # [ frontier, jetpos, shapepos ] -> [ shapenum, tmax ]

  towr, tmax, nrnd, nshp = Set[], -1, 0, 0
  while true do
    towr, tmax, nrnd, _nshp = fallrock(jets, towr, tmax, nrnd, nshp)

    # position already known ? ###################
    cachekey = [
      getfrontier(towr, tmax), (nshp % @shapes.length), (nrnd % jets.length)]
    match = cache.fetch(cachekey, nil)
    if match then
      prvnshp, prvtmax = match; cycleln = nshp - prvnshp
      return (prvtmax + (tmax - prvtmax) * (maxshpn - prvnshp) / cycleln) if
        ((maxshpn - prvnshp) % cycleln == 0)
    end
    cache[cachekey] = [nshp, tmax] if not match
    # ############################################

    nshp += 1
  end
end


# ##############################################################################
#
# 2022 DAY 16
#
# ##############################################################################

# Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
# Valve BB has flow rate=13; tunnels lead to valves CC, AA
# Valve CC has flow rate=2; tunnels lead to valves DD, BB
# Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
# Valve EE has flow rate=3; tunnels lead to valves FF, DD
# Valve FF has flow rate=0; tunnels lead to valves EE, GG
# Valve GG has flow rate=0; tunnels lead to valves FF, HH
# Valve HH has flow rate=22; tunnel leads to valve GG
# Valve II has flow rate=0; tunnels lead to valves AA, JJ
# Valve JJ has flow rate=21; tunnel leads to valve II

def scantunnels()
  input(2216).split("\n").map { |line|
    vnum, flow, *nexts = line.scan(/[0-9]+|[A-Z][A-Z]+/)
    [vnum, { flow: flow.to_i, nexts: nexts.to_set }] }.to_h
end

def valvemoveall(scan, pos, opened, timeleft, followed=false)
  # now let the elephant play !!
  return valvemoveall(scan, "AA", opened, 26) if timeleft == 0 and followed
  # no one to follow
  return 0 if timeleft == 0 and not followed

  # position already known
  @valvecache ||= {}
  match = @valvecache.fetch([pos, opened, timeleft, followed], nil)
  return match if match

  results = []
  if scan[pos][:flow] > 0 and not opened.include?(pos) then
    results << (
      ((timeleft - 1) * scan[pos][:flow]) +
      valvemoveall(scan, pos, (opened | Set[pos]), timeleft - 1, followed))
  end
  for succ in scan[pos][:nexts] do
    results << valvemoveall(scan, succ, opened, timeleft - 1, followed)
  end

  @valvecache[[pos, opened, timeleft, followed]] = results.max
end

# 1728 - took 11s
def d22161()
  valvemoveall(scantunnels(), "AA", Set[], 30)
end

# 2304 - took 440s!
def d22162()
  valvemoveall(scantunnels(), "AA", Set[], 26, true)
end

# TODO : look only at pathes between interesting valves


# ##############################################################################
#
# 2022 DAY 15
#
# ##############################################################################

#  ..........#.................
#  .........###................
#  ....S...#####...............
#  .......#######........S.....
#  ......#########S............
#  .....###########SB..........
#  ....#############...........
#  ...###############..........
#  ..#################.........
#  .#########S#######S#........
#  ..#################.........
#  ...###############..........
#  ....B############...........
#  ..S..###########............
#  ......#########.............
#  .......#######..............
#  ........#####.S.......S.....
#  B........###................
#  ..........#SB...............
#  ................S..........B
#  ....S.......................
#  ............................
#  ............S......S........
#  ............................
#  .......................B....

# 6275922
def d22151()
  sensors =
    input(2215).split("\n")
    .map { |line| line.scan(/[-0-9]+/).map(&:to_i).each_slice(2).to_a }.to_h

  rownum = 2000000

  xbeacons = sensors.values.filter_map{ |(x, y)| x if y == rownum }

  sensors
    .filter_map{ |(x1, y1), (x2, y2)|
      mndst = (x2 - x1).abs + (y2 - y1).abs
      delta = mndst - (rownum - y1).abs
      ((x1 - delta)..(x1 + delta)).to_a if delta >= 0
    }.flatten.then { _1 - xbeacons }.uniq.length
end

# 11747175442119
def d22152()
  sensors =
    input(2215).split("\n")
    .map { |line| line.scan(/[-0-9]+/).map(&:to_i).each_slice(2).to_a }
    .map { |(x1, y1), (x2, y2)| [[x1, y1], (x2 - x1).abs + (y2 - y1).abs] }
    .to_h

  maxcrd = 4000000

  (0..maxcrd).each{ |row|

    ranges =
      sensors.filter_map{ |(x, y), md|
      delta = md - (row - y).abs
      ((x - delta)..(x + delta)) if delta >= 0 }

    col = 0
    while col <= maxcrd do
      rng = ranges.find { |r| r.include?(col) }
      return (col * 4000000 + row) if not rng
      col = rng.end + 1
    end
  }
end


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
  input(2214)
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
