#!/usr/bin/env python3

import os, sys, re, math, subprocess
import operator, itertools, functools, collections
import timeit, random, time, builtins

assert sys.hexversion >= 0x03080000

# ###########################################################################

# exec(open('adv21.py').read()) ; d21011()


# ###########################################################################

# ###########################################################################
#
# 2020 DAY 3
#
# ###########################################################################

# 1071734
def d21031():
    input = getinput(2103).splitlines()
    nbbits = len(input[0])
    lst_mst = []
    for bitnum in range(nbbits):
        bitsn = list(l[bitnum] for l in input)
        bcnts = sorted(
            collections.Counter(bitsn).items(),
            key=operator.itemgetter(1))
        lst_mst.append(list(map(operator.itemgetter(0), bcnts)))
    eps, gam = list(int(''.join(l), 2) for l in map(list, zip(*lst_mst)))
    print(eps * gam)

# 6124992
def d21032():
    input = getinput(2103).splitlines()
    nbbits = len(input[0])
    def bitcrit(rank, lst):
        result = list(lst)
        for bitnum in range(nbbits):
            bitsn = list(l[bitnum] for l in result)
            bcnts = sorted(
                ({'0': 0, '1': 0} | dict(collections.Counter(bitsn))).items(),
                key=lambda k_v: (k_v[1], k_v[0]))
            bit = bcnts[rank][0]
            result = list(filter(lambda l: l[bitnum] == bit, result))
            if len(result) == 1: return int(result[0], 2)
        assert 0
    print(bitcrit(0, input) * bitcrit(1, input))


# ###########################################################################
#
# 2020 DAY 2
#
# ###########################################################################

# 1714950
def d21021():
    input = list(map(str.split, getinput(2102).splitlines()))
    hpos, depth = 0, 0
    for (d, n) in input:
        if d == 'forward':
            hpos += int(n)
        elif d == 'down':
            depth += int(n)
        elif d == 'up':
            depth -= int(n)
        else: assert 0
    print(hpos * depth)

# 1281977850
def d21022():
    input = list(map(str.split, getinput(2102).splitlines()))
    hpos, depth, aim = 0, 0, 0
    for (d, n) in input:
        if d == 'forward':
            hpos += int(n)
            depth += aim * int(n)
        elif d == 'down':
            aim += int(n)
        elif d == 'up':
            aim -= int(n)
        else: assert 0
    print(hpos * depth)


# ###########################################################################
#
# 2020 DAY 1
#
# ###########################################################################

# 1676
def d21011():
    input = list(map(int, getinput(2101).splitlines()))
    print(sum(x < y for x, y in zip(input, input[1:])))

# 1706
def d21012():
    input = list(map(int, getinput(2101).splitlines()))
    inpt3 = list(map(sum, zip(input, input[1:], input[2:])))
    print(sum(x < y for x, y in zip(inpt3, inpt3[1:])))

# # from reddit
# print(sum(x < y for x, y in zip(input, input[3:])))


# ###########################################################################

def getinput(id):
    return open(f'inputs/{id}.in').read()
