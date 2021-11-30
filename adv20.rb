#!/usr/bin/env ruby

# ##############################################################################

def debug(*args)
  puts "debug: " + args.map(&:to_s).join(" ")
  args.length == 1 ? args.first : args
end

# ##############################################################################

# load "adv20.rb" ; d20011()


# ###########################################################################
#
# 2020 DAY 09
#
# ###########################################################################

def input2009()
  IO.readlines('inputs/2009-0.in')
  .map(&:to_i)
end

# 69316178
def d20091()
  input, prmbl = input2009, 25
  prmbl
    .upto(input.length)
    .detect { |i|
    break input[i] if (
        input.slice(i - prmbl, prmbl)
          .combination(2)
          .detect{|a, b| a + b == input[i]}.nil?)}
end

def d20092()
  input = input2009
  target = 127

end

# ###########################################################################
#
# 2020 DAY 08
#
# ###########################################################################

def input2008()
  IO.readlines('inputs/2008.in', chomp: true)
  .map(&:split)
  .map.with_index{|(opc, arg), i| [i, [opc, arg.to_i]]}
  .to_h
end

def loop2008(input)
  pc, acc, seen = 0, 0, []
  while true do
    break if seen.member?(pc)
    seen.push(pc)
    break if not input.member?(pc)
    opc, arg = input[pc]
    if opc == "nop" then
      pc += 1
    elsif opc == "acc" then
      acc += arg
      pc += 1
    elsif opc == "jmp" then
      pc += arg
    else fail
    end
  end
  return acc, pc
end

# 2080
def d20081()
  loop2008(input2008).first
end

# 2477
def d20082()
  input = input2008()
  input
    .find{|i, (opc, arg)|
    if (opc == "nop")
      acc, pc = loop2008(input.merge({i => ["jmp", arg]}))
      break acc if not input.member?(pc)
    elsif (opc == "jmp")
      acc, pc = loop2008(input.merge({i => ["nop", arg]}))
      break acc if not input.member?(pc)
    else nil end
  }
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

def input2007()
  IO.readlines('inputs/2007.in', chomp: true)
    .map{ |l|
      src = l.scan(/^\w+ \w+/)
      dst = l.scan(/(\d+) (\w+ \w+)/).map{|n, b| [n.to_i, b]}
      [src.first, dst] }
    .to_h
end

# 229
def d20071()
  def isvalid?(input, color)
    (color == "shiny gold") or (input[color]&.find{|_, c| isvalid?(input, c)})
  end
  input2007
    .filter{|c, _| isvalid?(input2007, c) }
    .length - 1
end

# 6683
def d20072()
  def nbbags(input, color)
    input[color].map{|n, c| (n * (1 + nbbags(input, c)))}.sum
  end
  nbbags(input2007, "shiny gold")
end


# ###########################################################################
#
# 2020 DAY 06
#
# ###########################################################################

def input2006()
  IO.read('inputs/2006.in')
  .split("\n\n")
end

# 6947
def d20061()
  input2006()
    .map{|l| l.split.join.chars.uniq.length }
    .sum
end

# 3398
def d20062()
  input2006()
    .map{|l| l.scan(/\w+/).map{|s| s.chars}}
    .map{|l| l.reduce(:intersection).length }
    .sum
end


# ###########################################################################
#
# 2020 DAY 05
#
# ###########################################################################

def input2005()
  IO.read('inputs/2005.in').lines(chomp: true)
end

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
  return r1, s1
end

def seatid((row, seat)) row * 8 + seat end

# 947
def d20051()
  input2005()
    .map{|s| seatid(decode2005(s)) }
    .max
end

# 636
def d20052()
  boarded = input2005.map{|s| seatid(decode2005(s)) }
  ((boarded.min...boarded.max).to_a - boarded).first
end

# (1..126).to_a.product((1..7).to_a)
#   .map{|r, s| seatid([r, s]) }
#   .find{|sid| (not boarded.include?(sid) and boarded.include?(sid - 1) and
#                boarded.include?(sid + 1)) }


# ###########################################################################
#
# 2020 DAY 04
#
# ###########################################################################

def input2004()
  IO.read('inputs/2004.in')
  .split("\n\n")
  .map{|l| l.scan(/\S+/).map{|i| i.split(":")}.to_h }
end

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
  required = [
    "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]
  input2004
    .filter{|h| required.difference(h.keys).empty? }
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
  required = [
    "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

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

  input2004
    .filter{|h| required.difference(h.keys).empty? }
    .filter{|h| h.all?{|k, v| isvalid(k, v)} }
    .length
end


# ###########################################################################
#
# 2020 DAY 03
#
# ###########################################################################

def input2003()
  IO.readlines('inputs/2003.in', chomp: true)
end

# 254
def d20031()
  input = input2003()
  xmax, ymax = input.first.length, input.length
  slope_x, slope_y = 3, 1
  x, y, trees = 0, 0, 0
  while y < ymax do
    trees += 1 if (input[y][x % xmax] == '#')
    x, y = x + slope_x, y + slope_y
  end
  trees
end

# 1666768320
def d20032()
  input = input2003()
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
    .map{|slope| traverse.call(*slope) }
    .reduce(:*)
end


# ###########################################################################
#
# 2020 DAY 02
#
# ###########################################################################

def input2002()
  IO.readlines('inputs/2002.in').map{|l| l.scan(/\w+/)}
end

# 546
def d20021()
  input2002
    .filter{|lo, hi, c, s| (
              s.count(c).between?(lo.to_i, hi.to_i) )}
    .length
  # (lo.to_i .. hi.to_i).include?(s.count(c))
end

# 275
def d20022()
  input2002
    .filter{|lo, hi, c, s| (
              (s[lo.to_i - 1] == c) ^ (s[hi.to_i - 1] == c)) }
    .length
end


# ###########################################################################
#
# 2020 DAY 01
#
# ###########################################################################

def input2001()
  IO.read('inputs/2001.in').split.map(&:to_i)
end

# 605364
def d20011()
  input2001
  .combination(2)
  .find{|comb| comb.sum == 2020}
  .reduce(:*)
end

# 128397680
def d20012()
  input2001
    .combination(3)
    .find{|comb| comb.sum == 2020}
    .reduce(:*)
end


# ##############################################################################
