#!/usr/bin/env python3

import os, sys, re, math, subprocess
import operator, itertools, functools, collections
import timeit, random, time, builtins

assert sys.hexversion >= 0x03080000

# exec(open('adv21.py').read()) ; d21011()

# ###########################################################################

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
