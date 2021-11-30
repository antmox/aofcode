#!/usr/bin/env python3

import os, sys, re, operator, itertools
import timeit, random

# exec(open('adv18.py').read())

# ###########################################################################


# ###########################################################################
#
# 2018 DAY 6
#
# ###########################################################################

def mandist(x1, y1, x2, y2):
    return abs(x2 - x1) + abs(y2 - y1)


def d1861():
    lines = open('inputs/1806-0.in').readlines()
    input = list(
        map(lambda line: tuple(map(int, re.findall('\d+', line))),
            lines))
    xcoords = list(map(operator.itemgetter(0), input))
    ycoords = list(map(operator.itemgetter(1), input))
    xmin, xmax = min(xcoords), max(xcoords)
    ymin, ymax = min(ycoords), max(ycoords)

    def closest(x, y):
        distances = [(mandist(x, y, tx, ty), (tx, ty)) for (tx, ty) in input]
        distmin = sorted(distances)[0][0]
        closests = list(filter(lambda d_c: d_c[0] == distmin, distances))
        return closests[0][1] if (len(closests) == 1) else None

    cells = [(x, y) for x in range(xmin, xmax + 1) for y in range(ymin, ymax + 1)]
    grid = dict([((x, y), closest(x, y)) for (x, y) in cells])

    frontier = filter(
        lambda x_y: (x_y[0] in [xmin, xmax] or x_y[1] in [ymin, ymax]), cells)
    infinite = set(filter(bool, map(lambda x_y: grid[x_y], frontier)))

    print(xmin, xmax, ymin, ymax)
    print(cells)
    print(grid)
    print(infinite)


# ###########################################################################
#
# 2018 DAY 5
#
# ###########################################################################

def react(input):
    input = list(input)
    i = 0
    while True:
        if i == len(input) - 1:
            break
        elif abs(ord(input[i]) - ord(input[i + 1])) == 32:
            input.pop(i); input.pop(i)
            if i > 0: i -= 1
        else:
            i += 1
    return input

def d1851():
    input = open('inputs/1805.in').read().strip()

    return len(react(input))

def d1852():
    input = open('inputs/1805.in').read().strip()

    results = []
    for c in range(ord('a'), ord('z') + 1):
        simpl = input.replace(chr(c), '')
        simpl = simpl.replace(chr(c - 32), '')
        results.append(len(react(simpl)))

    return min(results)


# ###########################################################################
#
# 2018 DAY 4
#
# ###########################################################################

# [1518-09-27 00:40] wakes up
# [1518-04-10 23:52] Guard #3559 begins shift
# [1518-03-16 00:44] wakes up
# [1518-05-16 00:23] wakes up
# [1518-06-18 00:00] Guard #1499 begins shift
# [1518-11-21 00:48] wakes up
# [1518-05-19 00:29] falls asleep

def d1841():
    lines = open('inputs/1804.in').readlines()
    input = list(
        map(lambda line: tuple(re.findall('[\w]+', line)),
            lines))

    sleep = {}
    curr_id, curr_sleep = None, None

    for event in sorted(input):
        # ('1518', '11', '04', '23', '56', 'Guard', '3499', 'begins', 'shift')
        # ('1518', '11', '09', '00', '35', 'falls', 'asleep')
        # ('1518', '11', '15', '00', '57', 'wakes', 'up')
        if event[5] == 'Guard':
            curr_id = event[6]
        elif event[5] == 'falls':
            curr_sleep = int(event[4])
        elif event[5] == 'wakes':
            curr_wake = int(event[4])
            sleep.setdefault(curr_id, []).extend(range(curr_sleep, curr_wake))
        else: assert 0

    most = sorted(
        sleep.items(), key=lambda kv: len(kv[1]))[-1]

    minutes = sorted(
        map(lambda kv: (len(list(kv[1])), kv[0]),
            itertools.groupby(sorted(most[1]))))[-1][1]

    print( int(most[0]) * minutes)

def d1842():
    lines = open('inputs/1804.in').readlines()
    input = list(
        map(lambda line: tuple(re.findall('[\w]+', line)),
            lines))

    sleep = {}
    curr_id, curr_sleep = None, None

    for event in sorted(input):
        # ('1518', '11', '04', '23', '56', 'Guard', '3499', 'begins', 'shift')
        # ('1518', '11', '09', '00', '35', 'falls', 'asleep')
        # ('1518', '11', '15', '00', '57', 'wakes', 'up')
        if event[5] == 'Guard':
            curr_id = event[6]
        elif event[5] == 'falls':
            curr_sleep = int(event[4])
        elif event[5] == 'wakes':
            curr_wake = int(event[4])
            sleep.setdefault(curr_id, []).extend(range(curr_sleep, curr_wake))
        else: assert 0

    results = []
    for g, minutes in sleep.items():
        asleep = itertools.groupby(sorted(minutes))
        asleep = list(
            map(lambda kv: (len(list(kv[1])), kv[0], g), asleep))
        results.append(sorted(asleep)[-1])

    (nbm, vmn, gid) = sorted(results)[-1]
    return int(gid) * vmn


# ###########################################################################
#
# 2018 DAY 3
#
# ###########################################################################

#2 @ 192,174: 10x21
#3 @ 734,527: 23x10

def d1831():
    lines = open('inputs/1803.in').readlines()
    # lines = ['#1 @ 1,3: 4x4', '#2 @ 3,1: 4x4', '#3 @ 5,5: 2x2']
    input = list(
        map(lambda line: tuple(map(int, re.findall('\d+', line))),
            lines))

    grid = {}
    for (n, x, y, w, h) in input:
        cells = [(a, b) for a in range(x, x + w) for b in range(y, y + h)]
        for (a, b) in cells:
            grid.setdefault((a, b), []).append(n)

    return len(list(filter(lambda l: len(l) > 1, grid.values())))

def d1832():
    lines = open('inputs/1803.in').readlines()
    input = list(
        map(lambda line: tuple(map(int, re.findall('\d+', line))),
            lines))

    grid = {}
    for (n, x, y, w, h) in input:
        cells = [(a, b) for a in range(x, x + w) for b in range(y, y + h)]
        for (a, b) in cells:
            grid.setdefault((a, b), []).append(n)

    for (n, x, y, w, h) in input:
        values = [grid[(a, b)] for a in range(x, x + w) for b in range(y, y + h)]
        if not any(map(lambda l: len(l) > 1, values)):
            return n


# ###########################################################################
#
# 2018 DAY 2
#
# ###########################################################################

def d1821():
    input = list(
        map(lambda x: x.strip(),
            open('inputs/1802.in').readlines()))
    nb2, nb3 = 0, 0
    for boxid in input:
        groups = itertools.groupby(sorted(boxid))
        counts = list(map(lambda kv: len(list(kv[1])), groups))
        nb2 += (2 in counts) and 1 or 0
        nb3 += (3 in counts) and 1 or 0
    return nb2 * nb3

def d1822():
    input = list(
        map(lambda x: x.strip(),
            open('inputs/1802.in').readlines()))

    pairs = ((a, b) for a in range(len(input)) for b in range(a + 1, len(input)))
    longest = []
    for (a, b) in pairs:
        common = list(filter(lambda a_b: a_b[0] == a_b[1], zip(input[a], input[b])))
        if len(common) > len(longest): longest = common
    return ''.join(map (operator.itemgetter(1), longest))


# ###########################################################################
#
# 2018 DAY 1
#
# ###########################################################################

def d1811():
    input = list(map(int, open('inputs/1801.in').readlines()))
    return sum(input)

def d1812():
    input = list(map(int, open('inputs/1801.in').readlines()))
    seen = set()
    for f in itertools.accumulate(itertools.cycle(input)):
        if f in seen: break
        seen.add(f)
    return f


# ###########################################################################



