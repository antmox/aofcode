#!/usr/bin/env ruby

# ##############################################################################

# load "adv20.rb" ; d20011()

# ##############################################################################

# ###########################################################################
#
# 2020 DAY 10
#
# ###########################################################################

# 2070
def d20101()
  adapt =
    input(2010).split("\n").map(&:to_i).sort
      .then { |l| l.push(l.last + 3) }

  adapt.zip([0] + adapt).map {_1 - _2}.tally.values.reduce(:*)
end

# 24179327893504
def d20102()
  adapt =
    input(2010).split("\n").map(&:to_i).sort

  (adapt + [adapt.last + 3]).zip([0] + adapt)
    .map { _1 - _2 }
    .slice_when(&:!=)
    .filter { |x| x.first == 1 }
    .map(&:length)
    .map { |x| [0, 1, 2, 4, 7][x] }
    .reduce(&:*)
end


# ###########################################################################
#
# 2020 DAY 09
#
# ###########################################################################

# 69316178
def d20091()
  numbs, prmbl =
    input(2009).split("\n").map(&:to_i), 25

  numbs.each_cons(prmbl + 1) { |*lst, nb|
    lst.combination(2).detect { |a, b| a + b == nb } || (break nb) }
end

# 9351526
def d20092()
  numbs, targt =
    input(2009).split("\n").map(&:to_i), 69316178

  (0..numbs.length).detect { |start|
    stop = (start..numbs.length).detect { |stop|
      numbs[start..stop].sum == targt }
    break numbs[start..stop].minmax.sum if stop
  }
end


# ###########################################################################
#
# 2020 DAY 08
#
# ###########################################################################

def loop2008(progr)
  pc, acc, seen = 0, 0, []
  loop do
    break if seen.member?(pc) or not progr.member?(pc)
    seen.push(pc)
    opc, arg = progr[pc]
    if opc == "nop" then
      pc += 1
    elsif opc == "acc" then
      acc += arg
      pc += 1
    elsif opc == "jmp" then
      pc += arg
    else fail end
  end
  return acc, pc
end

# 2080
def d20081()
  progr =
    input(2008).split("\n").map(&:split)
      .map.with_index { |(opc, arg), i| [ i, [opc, arg.to_i] ] }
      .to_h

  loop2008(progr).first
end

# 2477
def d20082()
  progr =
    input(2008).split("\n").map(&:split)
      .map.with_index { |(opc, arg), i| [ i, [opc, arg.to_i] ] }
      .to_h

  progr.find { |i, (opc, arg)|
    if (opc == "nop")
      acc, pc = loop2008(progr.merge({ i => ["jmp", arg] }))
      break acc if not progr.member?(pc)
    elsif (opc == "jmp")
      acc, pc = loop2008(progr.merge({ i => ["nop", arg] }))
      break acc if not progr.member?(pc)
    else nil end }
end


# ###########################################################################
#
# 2020 DAY 07
#
# ###########################################################################

# light red bags contain 1 bright white bag, 2 muted yellow bags.
# dark orange bags contain 3 bright white bags, 4 muted yellow bags.
# bright white bags contain 1 shiny gold bag.
# muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
# shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
# dark olive bags contain 3 faded blue bags, 4 dotted black bags.
# vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
# faded blue bags contain no other bags.
# dotted black bags contain no other bags.

# 229
def d20071()
  rules = input(2007).split("\n")
    .map { |l|
      src = l.scan(/^\w+ \w+/).first
      dst = l.scan(/(\d+) (\w+ \w+)/).map { |n, b| [n.to_i, b] }
      [src, dst] }.to_h

  def isvalid?(rules, color)
    (color == "shiny gold") or rules[color].find { |_, c| isvalid?(rules, c) }
  end

  rules
    .filter { |c, _| isvalid?(rules, c) }
    .length - 1
end

# 6683
def d20072()
  rules = input(2007).split("\n")
    .map { |l|
      src = l.scan(/^\w+ \w+/).first
      dst = l.scan(/(\d+) (\w+ \w+)/).map { |n, b| [n.to_i, b] }
      [src, dst] }.to_h

  def nbbags(rules, color)
    rules[color].map { |n, c| n * (1 + nbbags(rules, c)) }.sum
  end

  nbbags(rules, "shiny gold")
end


# ###########################################################################
#
# 2020 DAY 06
#
# ###########################################################################

# 6947
def d20061()
  input(2006).split("\n\n")
    .map { |l| l.split.join.chars.uniq.length }
    .sum
end

# 3398
def d20062()
  input(2006).split("\n\n")
    .map { |l| l.scan(/\w+/).map(&:chars) }
    .map { |l| l.reduce(:intersection).length }
    .sum
end


# ###########################################################################
#
# 2020 DAY 05
#
# ###########################################################################

def decode2005(s)
  (r1, r2), (s1, s2) = [0, 127], [0, 7]
  for c in s.chars
    case c
    when "F" then r2 = (r1 + r2) / 2
    when "B" then r1 = 1 + (r1 + r2) / 2
    when "L" then s2 = (s1 + s2) / 2
    when "R" then s1 = 1 + (s1 + s2) / 2
    else fail
    end
  end
  fail unless (r1 == r2) and (s1 == s2)
  return r1 * 8 + s1
end

# 947
def d20051()
  input(2005).split("\n").map { |s| decode2005(s) }
    .max
end

# 636
def d20052()
  boarded =
    input(2005).split("\n").map { |s| decode2005(s) }
  ((boarded.min...boarded.max).to_a - boarded).first
end


# ###########################################################################
#
# 2020 DAY 04
#
# ###########################################################################

# byr (Birth Year)
# iyr (Issue Year)
# eyr (Expiration Year)
# hgt (Height)
# hcl (Hair Color)
# ecl (Eye Color)
# pid (Passport ID)
# cid (Country ID)

# 196
def d20041()
  passes =
    input(2004).split("\n\n")
      .map { |l| l.scan(/\S+/).map { |i| i.split(":") }.to_h }
  required =
    [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

  passes
    .filter { |h| required.difference(h.keys).empty? }
    .length
end

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#     If cm, the number must be at least 150 and at most 193.
#     If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

# 114
def d20042()
  passes =
    input(2004).split("\n\n")
      .map { |l| l.scan(/\S+/).map { |i| i.split(":") }.to_h }
  required =
    [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

  def isvalid(k, v)
    case k
    when "byr" then
      /^\d{4}$/.match?(v) and v.to_i.between?(1920, 2002)
    when "iyr" then
      /^\d{4}$/.match?(v) and v.to_i.between?(2010, 2020)
    when "eyr" then
      /^\d{4}$/.match?(v) and v.to_i.between?(2020, 2030)
    when "hgt" then
      /^\d+(in|cm)$/.match?(v) and
        if v.end_with?("cm") then v.to_i.between?(150, 193)
        elsif v.end_with?("in") then v.to_i.between?(59, 76) end
    when "hcl" then
      /^#[0-9a-f]{6}$/.match?(v)
    when "ecl" then
      ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].include?(v)
    when "pid" then
      /^\d{9}$/.match?(v)
    else
      true
    end
  end

  passes
    .filter { |h| required.difference(h.keys).empty? }
    .filter { |h| h.all? { |k, v| isvalid(k, v) } }
    .length
end


# ###########################################################################
#
# 2020 DAY 03
#
# ###########################################################################

# 254
def d20031()
  input =
    input(2003).split("\n")
  xmax, ymax =
    input.first.length, input.length

  traverse = lambda do |slope_x, slope_y|
    x, y, trees = 0, 0, 0
    while y < ymax do
      trees += 1 if (input[y][x % xmax] == '#')
      x, y = x + slope_x, y + slope_y
    end
    trees
  end

  traverse.call(3, 1)
end

# 1666768320
def d20032()
  input = input(2003).split("\n")
  xmax, ymax = input.first.length, input.length

  traverse = lambda do |slope_x, slope_y|
    x, y, trees = 0, 0, 0
    while y < ymax do
      trees += 1 if (input[y][x % xmax] == '#')
      x, y = x + slope_x, y + slope_y
    end
    trees
  end

  [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]]
    .map { |slope| traverse.call(*slope) }
    .reduce(:*)
end


# ###########################################################################
#
# 2020 DAY 02
#
# ###########################################################################

# 546
def d20021()
  input(2002)
    .split("\n").map { |l| l.scan(/\w+/) }
    .filter { |lo, hi, c, s|
      (s.count(c).between?(lo.to_i, hi.to_i) ) }
    .length
end

# 275
def d20022()
  input(2002)
    .split("\n").map { |l| l.scan(/\w+/) }
    .filter { |lo, hi, c, s|
      (s[lo.to_i - 1] == c) ^ (s[hi.to_i - 1] == c) }
    .length
end


# ###########################################################################
#
# 2020 DAY 01
#
# ###########################################################################

# 605364
def d20011()
  input(2001).split.map(&:to_i)
    .combination(2)
    .find { |comb| comb.sum == 2020 }
    .reduce(:*)
end

# 128397680
def d20012()
  input(2001).split.map(&:to_i)
    .combination(3)
    .find { |comb| comb.sum == 2020 }
    .reduce(:*)
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

fail if RUBY_VERSION.to_f < 2.7

if $PROGRAM_NAME == __FILE__

  proc, *args = ARGV

  proc ||= private_methods.filter {_1.to_s.match?(/d\d{5}/) }.sort.last

  debug("call #{proc} ( #{args} )")

  fstr = Time.now
  p self.send(proc.to_sym, *args)
  fend = Time.now

  debug("took #{fend - fstr}s")

end
