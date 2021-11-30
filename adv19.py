#!/usr/bin/env python3

import os, sys, re, math, subprocess
import operator, itertools, functools, collections
import timeit, random, time, builtins

assert sys.hexversion >= 0x03080000

# exec(open('adv19.py').read())

# ###########################################################################

# ###########################################################################
#
# 2019 DAY 25
#
# ###########################################################################

# 25165890
def d19251(input=None):
    input = input or open('inputs/1925.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))

    command, prevcmd = None, None
    def inputl():
        nonlocal command, prevcmd
        while True:
            cmd = command
            command = None; prevcmd = cmd
            print(f'>>> CMD={cmd}')
            for c in map(ord, cmd + '\n'):
                yield c

    generator = processIC23(
        input, inputl().__next__, yield_on_input=True)

    def visit(path):
        nonlocal outtmp, command
        prgs = outtmp.strip().split('\n\n')
        name, desc = prgs[0].splitlines()
        doors = list(map(lambda l: l[2:], prgs[1].splitlines()[1:]))
        items = list(map(lambda l: l[2:], prgs[2].splitlines()[1:]))

        od = {'north': 'south', 'south': 'north', 'west': 'east', 'east': 'west'}
        pdoor = od[path[-1]] if path else None
        if pdoor: doors.remove(pdoor)

        print(f'---------------------')
        print(f'{name=}')
        print(f'  {path=}')
        print(f'  {desc=}')
        print(f'  {doors=} {pdoor=}')
        print(f'  {items=}')

        for item in items:
            if item in ['escape pod', 'giant electromagnet', 'photons',
                        'molten lava', 'infinite loop']:
                continue
            yield f'take {item}'
            print('   ', (outtmp.strip().splitlines()[:],))

        for door in doors:
            yield door
            yield from visit(path + [door])
        if pdoor:
            yield pdoor

        if not path:
            yield from ['west', 'north', 'north', 'west']
            # analysis failure - room on west
            # drop all
            yield 'inv'
            invent = list(map(lambda i: i[2:], outtmp.strip().splitlines()[1:-2]))
            for i in invent: yield f'drop {i}'
            # try every combination
            for nbitems in range(1, len(invent) + 1):
                for comb in itertools.combinations(invent, nbitems):
                    print(f'{nbitems=} {comb=}')
                    for i in comb: yield f'take {i}'
                    yield 'west'
                    if 'Droids on this ship are lighter' in outtmp:
                        print('TOO HEAVY')
                    elif 'Droids on this ship are heavier' in outtmp:
                        print('TOO LIGHT')
                    else: return # END ???
                    for i in comb: yield f'drop {i}'

    visitor = visit([])
    outtmp = ''
    for n in itertools.count():
        try: out = next(generator)
        except StopIteration:
            print(outtmp); print('BYE'); break
        if not out is None: # output
            outtmp += chr(out)
            continue
        if not outtmp: # yielding
            continue
        # input
        try: command = next(visitor)
        except StopIteration: break
        outtmp = ''


# ###########################################################################
#
# 2019 DAY 24
#
# ###########################################################################

input241 = """
....#
#..#.
#..##
..#..
#....
"""

# 28781019
def d19241(input=None):
    input = input or open('inputs/1924.in').read()
    input = input.strip('\n').splitlines()
    cgrid = dict([((x, y), c) for (y, l) in enumerate(input) for (
        x, c) in enumerate(l)])
    csize = max(map(operator.itemgetter(0), cgrid.keys())) + 1

    def neigh(x, y):
        return filter(
            lambda x_y: all(map(lambda c: 0 <= c < csize, x_y)),
            [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)])

    def ngrid(cgrid):
        nxtgr = dict(cgrid)
        for x in range(csize):
            for y in range(csize):
                nbugs = len(list(filter(lambda c: cgrid[c] == '#', neigh(x, y))))
                if cgrid[(x, y)] == '#': n = '#' if (nbugs == 1) else '.'
                else: n = '#' if (nbugs == 1 or nbugs == 2) else '.'
                nxtgr[(x, y)] = n
        return nxtgr

    def strgrid(grid):
        s = ''
        for y in range(csize):
            for x in range(csize):
                s += grid.get((x, y))
            s += '\n'
        return s

    print(strgrid(cgrid))
    seen, sgrid = set([]), None
    while True:
        sgrid = strgrid(cgrid)
        if sgrid in seen: break
        seen.add(sgrid)
        cgrid = ngrid(cgrid)
    print(strgrid(cgrid))

    print(sum([2 ** (y * csize + x) for x in range(
        csize) for y in range(csize) if cgrid[(x, y)] == '#']))

# 1939
def d19242(input=None):
    input = input or open('inputs/1924.in').read()
    input = input.strip('\n').splitlines()
    cgrid = dict([((x, y, 0), c) for (y, l) in enumerate(input) for (
        x, c) in enumerate(l)])
    csize = max(map(operator.itemgetter(0), cgrid.keys())) + 1

    def neigh(x, y, l):
        ns = list(filter(
            lambda xyl: (0 <= xyl[0] < csize) and (0 <= xyl[1] < csize) and xyl[0:2] != (2, 2),
            [(x - 1, y, l), (x + 1, y, l), (x, y - 1, l), (x, y + 1, l)]))
        if x == 0: ns.append((1, 2, l - 1)) # XXX
        if y == 0: ns.append((2, 1, l - 1)) # XXX
        if x == 4: ns.append((3, 2, l - 1)) # XXX
        if y == 4: ns.append((2, 3, l - 1)) # XXX
        if (x, y) == (2, 1): # XXX
            ns.extend([(0, 0, l + 1), (1, 0, l + 1), (2, 0, l + 1), (3, 0, l + 1), (4, 0, l + 1)])
        if (x, y) == (2, 3): # XXX
            ns.extend([(0, 4, l + 1), (1, 4, l + 1), (2, 4, l + 1), (3, 4, l + 1), (4, 4, l + 1)])
        if (x, y) == (1, 2): # XXX
            ns.extend([(0, 0, l + 1), (0, 1, l + 1), (0, 2, l + 1), (0, 3, l + 1), (0, 4, l + 1)])
        if (x, y) == (3, 2): # XXX
            ns.extend([(4, 0, l + 1), (4, 1, l + 1), (4, 2, l + 1), (4, 3, l + 1), (4, 4, l + 1)])
        return ns

    def ngrid(cgrid):
        nxtgr = dict(cgrid)
        levels = list(map(operator.itemgetter(2), cgrid.keys()))
        for l in range(min(levels) - 1, max(levels) + 2):
            for x in range(csize):
                for y in range(csize):
                    nbugs = len(list(filter(lambda c: cgrid.get(c, None) == '#', neigh(x, y, l))))
                    if cgrid.get((x, y, l), None) == '#':
                        n = '#' if (nbugs == 1) else '.'
                    else:
                        n = '#' if (nbugs == 1 or nbugs == 2) else '.'
                    nxtgr[(x, y, l)] = n
            nxtgr[(2, 2, l)] = '?'
        return nxtgr

    for n in itertools.count():
        #
        if False:
            print(f'----- {n=} ------\n')
            levels = list(map(operator.itemgetter(2), cgrid.keys()))
            for l in range(min(levels), max(levels) + 1):
                if not any(filter(lambda k_v: k_v[0][2] == l and k_v[1] == '#', cgrid.items())): continue
                print(f'{n=} level={l}')
                for y in range(csize):
                    print(' ', end='')
                    for x in range(csize):
                        print('', cgrid.get((x, y, l), '?'), end='')
                    print()
                print()
            print(f'----------------')
        #
        nbugs = len(list(filter(lambda c: c == '#', cgrid.values())))
        print(f'{n=} {nbugs=}')
        if n == 200: break
        cgrid = ngrid(cgrid)

# n=0 level=0
#   . . . . #
#   # . . # .
#   # . . # #
#   . . # . .
#   # . . . .


# n=1 level=-1
#   . . . . .
#   . . # . .
#   . . ? # .
#   . . # . .
#   . . . . .

# n=1 level=0
#   # . . # .
#   # # # # .
#   # # ? . #
#   # # . # #
#   . # # . .

# n=1 level=1
#   . . . . #
#   . . . . #
#   . . ? . #
#   . . . . #
#   # # # # #


# ###########################################################################
#
# 2019 DAY 23
#
# ###########################################################################

def processIC23(prog, input_func, yield_on_input=False, dbg=False):

    def debug(*args):
        if not dbg: return
        print(*args)

    def param(pm, op):
        if pm == 0: return prog[op]
        elif pm == 1: return op
        elif pm == 2: return prog[rb + op]
        else: assert 0

    def paddr(pm, op):
        if pm == 0: return op
        elif pm == 2: return rb + op
        else: assert 0

    def pstr(pm, op):
        if pm == 0: return 'M[%d](%d)' % (op, prog[op])
        elif pm == 1: return 'I(%d)' % (op)
        elif pm == 2: return 'M[%d+%d](%d)' % (rb, op, prog[rb + op])
        else: assert 0

    pc, rb = 0, 0

    while True:
        pm3, pm2, pm1, *opc = ('%05d' % prog[pc])
        pm3, pm2, pm1 = int(pm3), int(pm2), int(pm1)
        opc = int(''.join(opc))

        if opc == 1: # ADD
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | ADD | %s = %s + %s (= %d)' % (
                pc, prog[pc], pstr(pm3, op3), pstr(pm1, op1), pstr(pm2, op2),
                param(pm1, op1) + param(pm2, op2)))
            prog[paddr(pm3, op3)] = param(pm1, op1) + param(pm2, op2)
            pc += 4

        elif opc == 2: # MUL
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | MUL | %s = %s * %s (= %d)' % (
                pc, prog[pc], pstr(pm3, op3), pstr(pm1, op1), pstr(pm2, op2),
                param(pm1, op1) * param(pm2, op2)))
            prog[paddr(pm3, op3)] = param(pm1, op1) * param(pm2, op2)
            pc += 4

        elif opc == 3: # INPUT
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 2]
            if yield_on_input: yield None
            input_ = input_func()
            debug('# PC=%05d OPC=%05d | INP | %s = (%s)' % (
                pc, prog[pc], pstr(pm1, op1), input_))
            prog[paddr(pm1, op1)] = input_
            pc += 2
            continue

        elif opc == 4: # OUTPUT
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 1, 2]
            output = param(pm1, op1)
            debug('# PC=%05d OPC=%05d | OUT | %s >>> %d' % (
                pc, prog[pc], pstr(pm1, op1), output))
            yield output
            pc += 2
            continue

        elif opc == 5: # JUMP_IF_TRUE
            op1, op2 = prog[pc + 1], prog[pc + 2]
            assert pm3 == 0 and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | JTR | if %s: goto %s' % (
                pc, prog[pc], pstr(pm1, op1), pstr(pm2, op2)))
            pc = param(pm2, op2) if param(pm1, op1) else (pc + 3)

        elif opc == 6: # JUMP_IF_FALSE
            op1, op2 = prog[pc + 1], prog[pc + 2]
            assert pm3 == 0 and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | JFL | if not %s: goto %s' % (
                pc, prog[pc], pstr(pm1, op1), pstr(pm2, op2)))
            pc = param(pm2, op2) if not param(pm1, op1) else (pc + 3)

        elif opc == 7: # LESS_THAN
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | LTH | %s = %s < %s (= %d)' % (
                pc, prog[pc], pstr(pm3, op3), pstr(pm1, op1), pstr(pm2, op2),
                1 if (param(pm1, op1) < param(pm2, op2)) else 0))
            prog[paddr(pm3, op3)] = 1 if (param(pm1, op1) < param(pm2, op2)) else 0
            pc += 4

        elif opc == 8: # EQUAL
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | EQU | %s = %s == %s (= %d)' % (
                pc, prog[pc], pstr(pm3, op3), pstr(pm1, op1), pstr(pm2, op2),
                1 if (param(pm1, op1) == param(pm2, op2)) else 0))
            prog[paddr(pm3, op3)] = 1 if (param(pm1, op1) == param(pm2, op2)) else 0
            pc += 4

        elif opc == 9: # ADJUST_RB
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | NRB | RB = %s + rb(%d) = %d' % (
                pc, prog[pc], pstr(pm1, op1), rb, rb + param(pm1, op1)))
            rb += param(pm1, op1)
            pc += 2

        elif opc == 99: # EXIT
            debug('# PC=%05d OPC=%05d | EXT' % (pc, opc))
            break

        else:
            debug('# PC=%05d OPC=%05d | ???' % (pc, opc))
            assert 0
    return

# 23213
def d19231(input=None):
    input = input or open('inputs/1923.in').read()
    input = list(map(int, input.strip().split(',')))
    nbcomputers = 50
    generators, inputs, outputs = {}, {}, {}
    def inputl(addr):
        while True:
            vlist = inputs.get(addr, None)
            value = vlist.pop(0) if vlist else -1
            print(f'# <INP {addr=} {value=}')
            yield value
    for addr in range(nbcomputers):
        inputs[addr] = [addr]
        input_ = collections.defaultdict(int, enumerate(input))
        generators[addr] = processIC23(
            input_, inputl(addr).__next__, yield_on_input=True)
    for n in itertools.count():
        for addr in range(nbcomputers):
            try: out = next(generators[addr])
            except StopIteration: raise
            # INPUT
            if out is None: continue
            # OUTPUT
            outputs.setdefault(addr, []).append(out)
            if len(outputs[addr]) < 3: continue
            # packet ready to be sent
            out1, out2, out3 = outputs[addr]
            print(f'# >OUT {addr=:03} [dest={out1:03} x={out2:03} y={out3:03}]')
            inputs.setdefault(out1, []).append(out2)
            inputs.setdefault(out1, []).append(out3)
            outputs[addr].clear()
            if out1 == 255: return # RESULT

# 17874
def d19232(input=None):
    input = input or open('inputs/1923.in').read()
    input = list(map(int, input.strip().split(',')))
    nbcomputers = 50
    generators, inputs, outputs = {}, {}, {}
    def inputl(addr):
        while True:
            vlist = inputs.get(addr, None)
            value = vlist.pop(0) if vlist else -1
            yield value
    for addr in range(nbcomputers):
        inputs[addr] = [addr]
        input_ = collections.defaultdict(int, enumerate(input))
        generators[addr] = processIC23(
            input_, inputl(addr).__next__, yield_on_input=True)
    waiting, idle_start, last_y = [], None, None
    for n in itertools.count():
        for addr in range(nbcomputers):
            try: out = next(generators[addr])
            except StopIteration: raise
            # INPUT
            if out is None:
                if addr in waiting:
                    pass
                elif not inputs.get(addr, None):
                    waiting.append(addr)
                continue
            # OUTPUT
            idle_start = None
            outputs.setdefault(addr, []).append(out)
            if len(outputs[addr]) < 3: continue
            # packet ready to be sent
            out1, out2, out3 = outputs[addr]
            outputs[addr].clear()
            if out1 < nbcomputers:
                inputs.setdefault(out1, []).append(out2)
                inputs.setdefault(out1, []).append(out3)
                if out1 in waiting: waiting.remove(out1)
            elif out1 == 255:
                inputs[255] = [out2, out3]
            else: assert 0
        #
        if len(waiting) != nbcomputers:
            idle_start = None
        elif not idle_start:
            idle_start = n
        elif (n - idle_start) >= 5:
            out1, out2, out3 = 0, *inputs[255]
            print(f'# IDLE {out3=}')
            if out3 == last_y: return
            last_y = out3
            inputs.setdefault(out1, []).append(out2)
            inputs.setdefault(out1, []).append(out3)
            inputs[255].clear()
            idle_start = None
        else: pass


# ###########################################################################
#
# 2019 DAY 22
#
# ###########################################################################

input221 = """
deal with increment 7
deal into new stack
deal into new stack
""" # 0 3 6 9 2 5 8 1 4 7

input222 = """
cut 6
deal with increment 7
deal into new stack
""" # 3 0 7 4 1 8 5 2 9 6

input223 = """
deal with increment 7
deal with increment 9
cut -2
""" # 6 3 0 7 4 1 8 5 2 9

input224 = """
deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
""" # 9 2 5 8 1 4 7 0 3 6

# 2939
def d19221(input=None, forder=10007, tcard=2019):
    input = input or open('inputs/1922.in').read()
    input = map(lambda l: l.split(), input.strip().splitlines())
    for w in input:
        if w[1] == 'into': # deal into new stack
            tcard = forder - tcard - 1
            print(f'> stk {tcard=}')
        elif w[0] == 'cut': # cut N
            tcard = (tcard - int(w[1])) % forder
            print(f'> cut {tcard=}')
        elif w[1] == 'with': # deal with increment N
            tcard = (tcard * int(w[3])) % forder
            print(f'> inc {tcard=}')
        else: assert 0
    print(f'{tcard=}')

# deal into new stack
#   fx = -1.x -1
# cut N
#   fx = x - N
# deal with increment N
#   fx = N.x

# shuff(0) = 4240
# shuff(1) = 8601
# shuff(x) = (4361 x + 4240) % forder

# 45347150615590
def d19222(input=None, forder=119315717514047, tcard=2020, repeat=101741582076661):
    input = input or open('inputs/1922.in').read()
    input = list(map(lambda l: l.split(), input.strip().splitlines()))
    A, B = 1, 0
    for w in input:
        if w[1] == 'into': # (inv of) deal into new stack
            A = (A * -1) % forder
            B = (B + A) % forder
        elif w[0] == 'cut': # inv of cut N
            B = (B + A * int(w[1])) % forder
        elif w[1] == 'with': # inf of deal with increment N
            A = (A * pow(int(w[3]), forder - 2, forder)) % forder
            # mod inverse on primes
        else: assert 0
    print(f'{A=} {B=}')
    # so for inverting one shuffle : x = (Ay + B) % forder
    # then for inverting n shuffle:
    bigA = pow(A, repeat, forder) % forder
    bigB = B * (1 - bigA) * pow(1 - A, forder-2, forder) % forder
    print(f'{bigA=} {bigB=}')
    print(f'> res = {(bigA * tcard + bigB) % forder}')
    # XXX https://old.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/fbnkaju


# ###########################################################################
#
# 2019 DAY 21
#
# ###########################################################################

# !(A && B && C) && D
# (!A || !B || !C) && D

# NOT A T
# NOT B J
# OR J T
# NOT C J
# OR T J
# AND D J

# 19350375
def d19211(input=None):
    input = input or open('inputs/1921.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    def inputl():
        prog = ['NOT A T', 'NOT B J', 'OR J T', 'NOT C J', 'OR T J', 'AND D J']
        yield from map(ord, '\n'.join(prog + ['WALK', '']))
    generator = processIC13(input, inputl().__next__, False)
    for n in itertools.count():
        try: out = next(generator)
        except StopIteration: break
        print(chr(out) if (out < 256) else '', end='')
    print('>', out)

# 1 2 3 4 5 6 7 8 9
# A B C D E F G H I
# ? ? ? # . # # . #

# (!A || !B || !C) && D && (E || H)

# NOT E T
# NOT T T
# OR H T
# AND T J

# 1143990055
def d19212(input=None):
    input = input or open('inputs/1921.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    def inputl():
        prog = ['NOT A T', 'NOT B J', 'OR J T', 'NOT C J', 'OR T J', 'AND D J',
                'NOT E T', 'NOT T T', 'OR H T', 'AND T J']
        yield from map(ord, '\n'.join(prog + ['RUN', '']))
    generator = processIC13(input, inputl().__next__, False)
    for n in itertools.count():
        try: out = next(generator)
        except StopIteration: break
        print(chr(out) if (out < 256) else '', end='')
    print('>', out)


# ###########################################################################
#
# 2019 DAY 20
#
# ###########################################################################

# 23 steps (26 wo portals)
input201 = """
         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
DE..#######...###.#
  #.#########.###.#
FG..#########.....#
  ###########.#####
             Z
             Z
"""

# 570
def d19201(input=None):
    input = input or open('inputs/1920.in').read()
    input = input.strip('\n').splitlines()
    maze = dict([((x, y), c) for (y, l) in enumerate(input) for (
        x, c) in enumerate(l)])
    # portals
    max_x = max(map(operator.itemgetter(0), maze.keys()))
    max_y = max(map(operator.itemgetter(1), maze.keys()))
    prtmp, portals = {}, {}
    for y in range(max_y + 1):
        for x in range(max_x + 1):
            if not ((l1 := maze.get((x, y), '')).isalpha()):
                continue
            l2 = c = None
            if ((l2 := maze.get((x + 1, y), '')).isalpha()):
                if maze.get((x + 2, y), None) == '.': c = (x + 2, y)
                elif maze.get((x - 1, y), None) == '.': c = (x - 1, y)
            elif ((l2 := maze.get((x, y + 1), '')).isalpha()):
                if maze.get((x, y + 2), None) == '.': c = (x, y + 2)
                elif maze.get((x, y - 1), None) == '.': c = (x, y - 1)
            if not l2 or not c: continue
            prtmp.setdefault(l1 + l2, []).append(c)
    for (prid, lst) in prtmp.items():
        el1 = prid if (prid in ['AA', 'ZZ']) else lst[1]
        portals[lst[0]] = el1
        portals[el1] = lst[0]
    # bfs
    to_visit, visited = [(portals['AA'], 0)], []
    while to_visit:
        ((x, y), d) = to_visit.pop(0)
        visited.append((x, y))
        for nxt in [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]:
            if nxt in visited: continue
            val = maze.get(nxt, None)
            if val == '#': continue
            if val != '.': continue
            out = portals.get(nxt, None)
            if out == 'ZZ':
                print(f'>>> res={d+1}'); return
            elif out:
                to_visit.append(((*out,), d + 2))
            else:
                to_visit.append(((*nxt,), d + 1))

# 396 steps
input202 = """
             Z L X W       C
             Z P Q B       K
  ###########.#.#.#.#######.###############
  #...#.......#.#.......#.#.......#.#.#...#
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###
  #.#...#.#.#...#.#.#...#...#...#.#.......#
  #.###.#######.###.###.#.###.###.#.#######
  #...#.......#.#...#...#.............#...#
  #.#########.#######.#.#######.#######.###
  #...#.#    F       R I       Z    #.#.#.#
  #.###.#    D       E C       H    #.#.#.#
  #.#...#                           #...#.#
  #.###.#                           #.###.#
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#
CJ......#                           #.....#
  #######                           #######
  #.#....CK                         #......IC
  #.###.#                           #.###.#
  #.....#                           #...#.#
  ###.###                           #.#.#.#
XF....#.#                         RF..#.#.#
  #####.#                           #######
  #......CJ                       NM..#...#
  ###.#.#                           #.###.#
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#
  #.....#        F   Q       P      #.#.#.#
  ###.###########.###.#######.#########.###
  #.....#...#.....#.......#...#.....#.#...#
  #####.#.###.#######.#######.###.###.#.#.#
  #.......#.......#.#.#.#.#...#...#...#.#.#
  #####.###.#####.#.#.#.#.###.###.#.###.###
  #.......#.....#.#...#...............#...#
  #############.#.#.###.###################
               A O F   N
               A A D   M
"""

# 7056
def d19202(input=None):
    input = input or open('inputs/1920.in').read()
    input = input.strip('\n').splitlines()
    maze = dict([((x, y), c) for (y, l) in enumerate(input) for (
        x, c) in enumerate(l)])
    # portals
    max_x = max(map(operator.itemgetter(0), maze.keys()))
    max_y = max(map(operator.itemgetter(1), maze.keys()))
    prtmp, outprt, inprt = {}, {}, {}
    for y in range(max_y + 1):
        for x in range(max_x + 1):
            if not ((l1 := maze.get((x, y), '')).isalpha()):
                continue
            l2 = c = None
            if ((l2 := maze.get((x + 1, y), '')).isalpha()):
                if maze.get((x + 2, y), None) == '.': c = (x + 2, y)
                elif maze.get((x - 1, y), None) == '.': c = (x - 1, y)
            elif ((l2 := maze.get((x, y + 1), '')).isalpha()):
                if maze.get((x, y + 2), None) == '.': c = (x, y + 2)
                elif maze.get((x, y - 1), None) == '.': c = (x, y - 1)
            if not l2 or not c: continue
            prtmp.setdefault(l1 + l2, []).append(c)
    for (prid, lst) in prtmp.items():
        if prid in ['AA', 'ZZ']:
            outprt[prid] = lst[0]
            outprt[lst[0]] = prid
            continue
        if (2 < lst[0][0] < max_x - 2) and (2 < lst[0][1] < max_y - 2):
            inprt[lst[0]] = lst[1]
        else:
            outprt[lst[0]] = lst[1]
        if (2 < lst[1][0] < max_x - 2) and (2 < lst[1][1] < max_y - 2):
            inprt[lst[1]] = lst[0]
        else:
            outprt[lst[1]] = lst[0]
    # print maze
    for y in range(max_y + 1):
        for x in range(max_x + 1):
            if (x, y) in outprt: print('O', end='')
            elif (x, y) in inprt: print('I', end='')
            else: print(maze.get((x, y), ' '), end='')
        print()
    #
    @functools.lru_cache(maxsize=None)
    def avpex(x, y, lzero):
        visited, tovisit, exts = set([]), [(x, y, 0)], []
        while True:
            (x, y, d) = tovisit.pop(0)
            for (nx, ny) in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]:
                if (nx, ny) in visited: continue
                value = maze.get((nx, ny), None)
                if value == '#': continue
                if value != '.': continue
                out = outprt.get((nx, ny), None)
                if out == 'ZZ':
                    if lzero: exts.append((nx, ny, d + 1, 0))
                elif out == 'AA':
                    pass # always blocked
                elif out:
                    if not lzero: exts.append((*out, d + 2, -1))
                elif inr := inprt.get((nx, ny), None):
                    exts.append((*inr, d + 2, 1))
                else:
                    tovisit.append((nx, ny, d + 1))
            visited.add((x, y))
            if not tovisit: break
        return exts
    #
    tovisit, visited = [(outprt['AA'], 0, 0)], []
    while tovisit:
        ((x, y), d, l) = tovisit.pop(0)
        for (nx, ny, dd, dl) in avpex(x, y, not l):
            out = outprt.get((nx, ny), None)
            if out == 'ZZ': print('XXX', d + dd); return
            tovisit.append(((nx, ny), d + dd, l + dl))


# ###########################################################################
#
# 2019 DAY 19
#
# ###########################################################################

def print_grid(grid):
    max_x = max(map(operator.itemgetter(0), grid.keys()))
    max_y = max(map(operator.itemgetter(1), grid.keys()))
    for y in range(max_y + 1):
        vs = []
        for x in range(max_x + 1):
            v = grid.get((x, y), 0)
            vs.append(v)
            print(v and '#' or '.', end='')
        if y >= 2: print(y, vs.index(1), vs.index(1) + vs.count(1), vs.count(1) )
        else: print()
    print()

# 199
def d19191(input=None):
    input = input or open('inputs/1919.in').read()
    input = list(map(int, input.strip().split(',')))
    grid = {}
    for y in range(0, 50):
        for x in range(0, 50):
            inputl = [x, y]
            def inputf(): return inputl.pop(0)
            generator = processIC13(
                collections.defaultdict(int, enumerate(input)), inputf, False)
            try: out1 = next(generator)
            except StopIteration: break
            grid[(x, y)] = out1
    print_grid(grid)
    print(len(list(filter(lambda x: x == 1, grid.values()))))

def d19192(input=None):
    input = input or open('inputs/1919.in').read()
    input = list(map(int, input.strip().split(',')))

    def compv(x, y, dbg=False):
        def inputf(): yield from [x, y]
        generator = processIC13(
            collections.defaultdict(
                int, enumerate(input)), inputf().__next__, dbg)
        return next(generator)

    (x, y) = (3, 2)
    while True:
        print(f'{x=} {y=}')
        if not compv(x, y):
            x += 1
        elif not compv(x + 99, y):
            y += 1
        elif not compv(x, y + 99):
            x += 1
        else:
            break
    print(f'res = {x*10000+y}')

    # (x, y) in beam
    # (x + 99, y) in beam
    # (x, y + 99) in beam
    #
    # ...#################.......
    # ....########XOOOOOOOOX.....
    # .....#######OOOOOOOOOO#....
    # ......######OOOOOOOOOO###..
    # .......#####OOOOOOOOOO#####
    # ........####OOOOOOOOOO#####
    # ........####OOOOOOOOOO#####
    # .........###OOOOOOOOOO#####
    # ..........##OOOOOOOOOO#####
    # ...........#OOOOOOOOOO#####
    # ............XOOOOOOOOO#####


# ###########################################################################
#
# 2019 DAY 18
#
# ###########################################################################

# 86 steps (a, b, c, d, e, f)
input182 = """
########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
"""

# 132 steps (b, a, c, d, f, e, g)
input183 = """
########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################
"""

# 136 steps (a, f, b, j, g, n, h, d, l, o, e, p, c, i, k, m)
input184 = """
#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################
"""

# 81 steps ( a, c, f, i, d, g, b, e, h )
input185 = """
########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################
"""

# 8 steps (a, b)
input181 = """
#########
#b.A.@.a#
#########
"""

# 6286
def d19181(input=None):
    input = input or open('inputs/1918.in').read()
    input = list(
        filter(bool, map(lambda l: l.strip(), input.splitlines())))
    area = dict([((x, y), c) for (y, l) in enumerate(input) for (x, c) in enumerate(l)])
    x, y = list(filter(lambda k_v: k_v[1] == '@', area.items()))[0][0]
    missing = ''.join(sorted(map(lambda p_k: p_k[1], filter(lambda k_v: k_v[1].islower(), area.items()))))
    gates = dict(list(map(lambda p_k: (p_k[1], p_k[0]), filter(lambda k_v: k_v[1].isupper(), area.items()))))
    area.update({(x, y): '.'})

    @functools.lru_cache(maxsize=None)
    def avkeys(missing, x, y): # bfs
        visited, tovisit, keys = set([]), [(x, y, 0)], []
        while True:
            (x, y, d) = tovisit.pop(0)
            for (nx, ny) in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]:
                if (nx, ny) in visited: continue
                value = area[(nx, ny)]
                if value == '.':
                    tovisit.append((nx, ny, d + 1))
                elif value.isupper():
                    if value.lower() not in missing:
                        tovisit.append((nx, ny, d + 1))
                    else: pass # blocked
                elif value.islower():
                    if value in missing:
                        keys.append((value, nx, ny, d + 1))
                    else:
                        tovisit.append((nx, ny, d + 1))
                else: pass # blocked
            visited.add((x, y))
            if not tovisit: break
        return keys

    @functools.lru_cache(maxsize=None)
    def trypath(missing, x, y, d):
        result = None
        for (k, kx, ky, kd) in avkeys(missing, x, y):
            kidx = missing.index(k)
            nmissing = missing[:kidx] + missing[kidx+1:]
            res = (d + kd if not nmissing else
                   trypath(nmissing, kx, ky, d + kd))
            result = result and min(res, result) or res
        return result

    print(trypath(missing, x, y, 0))

# 8 steps
input186 = """
#######
#a.#Cd#
##...##
##.@.##
##...##
#cB#Ab#
#######
"""

# 24 steps
input187 = """
###############
#d.ABC.#.....a#
###### # ######
#######@#######
###### # ######
#b.....#.....c#
###############
"""

# 32 steps
input188 = """
#############
#DcBa.#.GhKl#
#.### # #I###
#e#d##@##j#k#
###C# # ###J#
#fEbA.#.FgHi#
#############
"""

# 72 steps
input189 = """
#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba.#.BcIJ#
######@######
#nK.L.#.G...#
#M###N#H###.#
#o#m..#i#jk.#
#############
"""

# 2140
def d19182(input=None):
    input = input or open('inputs/1918.in').read()
    input = list(
        filter(bool, map(lambda l: l.strip(), input.splitlines())))
    area = dict([((x, y), c) for (y, l) in enumerate(input) for (x, c) in enumerate(l)])
    x, y = list(filter(lambda k_v: k_v[1] == '@', area.items()))[0][0]
    missing = ''.join(sorted(map(lambda p_k: p_k[1], filter(
        lambda k_v: k_v[1].islower(), area.items()))))
    gates = dict(list(map(lambda p_k: (p_k[1], p_k[0]), filter(
        lambda k_v: k_v[1].isupper(), area.items()))))
    # map update
    area.update({
        (x, y): '#', (x + 1, y): '#', (x - 1, y): '#',
        (x, y + 1): '#', (x, y - 1): '#'})
    (x1, y1, x2, y2, x3, y3, x4, y4) = (
        x - 1, y - 1, x - 1, y + 1, x + 1, y - 1, x + 1, y + 1)

    # def printarea(area, xys):
    #     max_x = max(map(operator.itemgetter(0), area.keys()))
    #     max_y = max(map(operator.itemgetter(1), area.keys()))
    #     for y in range(max_y + 1):
    #         for x in range(max_x + 1):
    #             if (x, y) in xys: print('@', end='')
    #             else: print(f'{area[(x, y)]}', end='')
    #         print()
    # printarea(area, [(x1, y1), (x2, y2), (x3, y3), (x4, y4)])

    @functools.lru_cache(maxsize=None)
    def avkeys(missing, x, y): # bfs
        visited, tovisit, keys = set([]), [(x, y, 0)], []
        while True:
            (x, y, d) = tovisit.pop(0)
            for (nx, ny) in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]:
                if (nx, ny) in visited: continue
                value = area[(nx, ny)]
                if value == '.':
                    tovisit.append((nx, ny, d + 1))
                elif value.isupper():
                    if value.lower() not in missing:
                        tovisit.append((nx, ny, d + 1))
                    else: pass # blocked
                elif value.islower():
                    if value in missing:
                        keys.append((value, nx, ny, d + 1))
                    else:
                        tovisit.append((nx, ny, d + 1))
                else: pass # blocked
            visited.add((x, y))
            if not tovisit: break
        return keys

    @functools.lru_cache(maxsize=None)
    def trypath4(missing, x1, y1, x2, y2, x3, y3, x4, y4, d):
        result = None

        for (k, kx, ky, kd) in avkeys(missing, x1, y1):
            kidx = missing.index(k)
            nmissing = missing[:kidx] + missing[kidx+1:]
            res = (d + kd if not nmissing else
                   trypath4(nmissing, kx, ky, x2, y2, x3, y3, x4, y4, d + kd))
            result = result and min(res, result) or res

        for (k, kx, ky, kd) in avkeys(missing, x2, y2):
            kidx = missing.index(k)
            nmissing = missing[:kidx] + missing[kidx+1:]
            res = (d + kd if not nmissing else
                   trypath4(nmissing, x1, y1, kx, ky, x3, y3, x4, y4, d + kd))
            result = result and min(res, result) or res

        for (k, kx, ky, kd) in avkeys(missing, x3, y3):
            kidx = missing.index(k)
            nmissing = missing[:kidx] + missing[kidx+1:]
            res = (d + kd if not nmissing else
                   trypath4(nmissing, x1, y1, x2, y2, kx, ky, x4, y4, d + kd))
            result = result and min(res, result) or res

        for (k, kx, ky, kd) in avkeys(missing, x4, y4):
            kidx = missing.index(k)
            nmissing = missing[:kidx] + missing[kidx+1:]
            res = (d + kd if not nmissing else
                   trypath4(nmissing, x1, y1, x2, y2, x3, y3, kx, ky, d + kd))
            result = result and min(res, result) or res

        return result

    print(trypath4(missing, x1, y1, x2, y2, x3, y3, x4, y4, 0))


# ###########################################################################
#
# 2019 DAY 17
#
# ###########################################################################

# 4800
def d19171(input=None):
    input = input or open('inputs/1917.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    def inputf(): return None
    generator = processIC13(input, inputf, False)
    grid, x, y = {}, 0, 0
    for n in itertools.count():
        try: out = next(generator)
        except StopIteration: break
        out = chr(out)
        if out == '.' or out == 'X':
            grid[(x, y)] = 0
            (x, y) = (x + 1, y)
        elif out == '#' or out in ['^', 'v', '<', '>']:
            grid[(x, y)] = 1
            (x, y) = (x + 1, y)
        elif out == '\n':
            (x, y) = (0, y + 1)
        else: assert 0
        print(out, end='')
    print()

    sumal = 0
    max_x = max(map(operator.itemgetter(0), grid.keys()))
    max_y = max(map(operator.itemgetter(1), grid.keys()))
    for y in range(max_y + 1):
        for x in range(max_x + 1):
            v = grid.get((x, y), 0)
            if (v == 1
                and grid.get((x - 1, y), 0) != 0
                and grid.get((x + 1, y), 0) != 0
                and grid.get((x, y - 1), 0) != 0
                and grid.get((x , y + 1), 0) != 0):
                # print('O', end='')
                sumal += (x * y)
            # elif v == 0: print('.', end='') # empty
            # elif v == 1: print('#', end='') # wall
        # print()
    print('# sum algn', sumal)


# ..............#########..........................
# ..............#.......#..........................
# ..............#.......#..........................
# ..............#.......#..........................
# ..............#.......#..........................
# ..............#.......#..........................
# ..............#.......#..........................
# ..............#.......#..........................
# ..............#...#####.#######.....#############
# ..............#...#.....#.....#.....#...........#
# ..............#...#.....#.....#.....#...........#
# ..............#...#.....#.....#.....#...........#
# ..............#######...#...#############.......#
# ..................#.#...#...#.#.....#...#.......#
# ..................#.#...#############...#.......#
# ..................#.#.......#.#.........#.......#
# ..................#.#.......#.#.........#########
# ..................#.#.......#.#..................
# ..............#############.#.#...#######........
# ..............#...#.#.....#.#.#...#.....#........
# ############^.#...#############...#.....#........
# #.............#.....#.....#.#.....#.....#........
# #.............#.....#.....#.#.....#.....#........
# #.............#.....#.....#.#.....#.....#........
# #.............#######.....#.#############........
# #.........................#.......#..............
# #.....#########...........#.......#..............
# #.....#.......#...........#.......#..............
# #.....#.......#...........#.......#..............
# #.....#.......#...........#.......#..............
# #.....#.......#############.......#######........
# #.....#.................................#........
# #######.................................#........
# ........................................#........
# ........................................#........
# ........................................#........
# ........................................#........
# ........................................#........
# ........................................#........
# ........................................#........
# ........................................#........
# ........................................#........
# ................................#########........

# L12 L12 L6 L6 R8 R4 L12 L12 L12 L6 L6 L12 L6
# R12 R8 R8 R4 L12 L12 L12 L6 L6 L12 L6 R12 R8
# R8 R4 L12 L12 L12 L6 L6 L12 L6 R12 R8

# B C B A C B A C B A
# L 12 L 6 R 12 R 8
# L 12 L 12 L 6 L 6
# R 8 R 4 L 12

# 982279
def d19172(input=None):
    input = input or open('inputs/1917.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    input.update({0: 2}) # wake up the robot
    def inputl():
        yield from [
            66, 44, 67, 44, 66, 44, 65, 44, 67, 44, 66, 44, 65, 44, 67, 44, 66, 44, 65, 10,
            76, 44, 49, 50, 44, 76, 44, 54, 44, 82, 44, 49, 50, 44, 82, 44, 56, 10,
            76, 44, 49, 50, 44, 76, 44, 49, 50, 44, 76, 44, 54, 44, 76, 44, 54, 10,
            82, 44, 56, 44, 82, 44, 52, 44, 76, 44, 49, 50, 10,
            110, 10
        ]
    generator = processIC13(input, inputl().__next__, False)
    for n in itertools.count():
        try: out = next(generator)
        except StopIteration: break
        print((out < 256) and chr(out) or '', end='')
    print('>', out)


# ###########################################################################
#
# 2019 DAY 16
#
# ###########################################################################

input161 = '12345678'

# 80871224585914546619083218645595 becomes 24176176
input162 = '80871224585914546619083218645595'

# 19617804207202209144916044189917 becomes 73745418
input163 = '19617804207202209144916044189917'

# 69317163492948606335995924319873 becomes 52432133
input164 = '69317163492948606335995924319873'

# 90744714
def d19161(input=None, nbphase=100):
    input = input or open('inputs/1916.in').read().strip()
    input = list(map(int, str(input)))
    pattern = [0, 1, 0, -1]
    for phase in range(nbphase):
        newinp = []
        for d in range(len(input)):
            s = 0
            for (n, i) in enumerate(input):
                s += i * pattern[(((n + 1) // (d + 1))) % len(pattern)]
            s = abs(s) % 10
            newinp.append(s)
        input = newinp
    print('>', ''.join(map(str,newinp[:8])))


# 03036732577212944063491565474664 becomes 84462026
input165 = '03036732577212944063491565474664'

# 02935109699940807407585447034323 becomes 78725270
input166 = '02935109699940807407585447034323'

# 03081770884921959731165446850517 becomes 53553731
input167 = '03081770884921959731165446850517'

#  p1    p2    p3    p4    p5    p6    p7    p8
#  1.i1  0.i2 -1.i3  0.i4  1.i5  0.i6 -1.i7  0.i8  n1
#  0.i1  1.i2  1.i3  0.i4  0.i5 -1.i6 -1.i7  0.i8  n2
#  0.i1  0.i2  1.i3  1.i4  1.i5  0.i6  0.i7  0.i8  n3
#  0.i1  0.i2  0.i3  1.i4  1.i5  1.i6  1.i7  0.i8  n4
#                          1.i5  1.i6  1.i7  1.i8  n5   = i5 + i6 + i7 + i8
#                                1.i6  1.i7  1.i8  n6   = i6 + i7 + i8
#                                      1.i7  1.i8  n7   = i7 + i8
#                                            1.i8  n8   = i8

# 82994322
def d19162(input=None, repeat=10000, nbphase=100):
    input = input or open('inputs/1916.in').read().strip()
    input = list(map(int, str(input))) * repeat
    offset = int(''.join(map(str, input[:7])))
    assert (offset * 2) >= len(input) # only valid for 2nd half
    input = list(reversed(input[offset:]))
    for phase in range(nbphase):
        newinp, s = [], 0
        for d in input:
            s = abs(s + d) % 10
            newinp.append(s)
        input = newinp
    print('>', ''.join(map(str,list(reversed(newinp))[:8])))


# ###########################################################################
#
# 2019 DAY 15
#
# ###########################################################################

def print_area(area, droid_pos):
    all_xs = list(map(operator.itemgetter(0), area.keys()))
    all_ys = list(map(operator.itemgetter(1), area.keys()))
    min_x, max_x = min(all_xs), max(all_xs)
    min_y, max_y = min(all_ys), max(all_ys)
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            v = area.get((x, y), None)
            if (x, y) == droid_pos: print('D', end='') # droid
            elif v == None: print(' ', end='') # unknown
            elif v == 0: print('#', end='') # wall
            elif v == 1: print('.', end='') # empty
            elif v == 2: print('X', end='') # oxygen
        print()
    print('droid', droid_pos)

# 404
def d19151(input=None):
    input = input or open('inputs/1915.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))

    droid_pos, droid_dir = (0, 0), None
    area, oxygen = {droid_pos: 1}, None

    def nextpos(droid_pos, droid_dir):
        mov = {1: (0, -1), 2: (0, 1), 3: (-1, 0), 4: (1, 0)}[droid_dir]
        return (droid_pos[0] + mov[0], droid_pos[1] + mov[1])

    def move_generator(prev_dir):
        nonlocal droid_dir, droid_pos, area, move_gen
        for m in [1, 4, 2, 3]:
            nxt = nextpos(droid_pos, m)
            val = area.get(nxt, None)
            if val != None: continue # we already know it
            yield m
            if droid_pos != nxt: continue # wall
            yield from move_generator(m)
        if not prev_dir: return # top level generator
        back_dir = {1:2, 2:1, 3:4, 4:3}[prev_dir]
        yield back_dir

    def inputf(): # 1:N 2:S 3:W 4:E
        nonlocal droid_dir, droid_pos, area, move_gen
        droid_dir = next(move_gen)
        # print('>>>', droid_dir)
        return droid_dir

    move_gen = move_generator(droid_dir)
    generator = processIC13(input, inputf, False)

    for n in itertools.count():
        try: out1 = next(generator)
        except StopIteration: break
        looked = nextpos(droid_pos, droid_dir)
        area[looked] = out1
        if out1 != 0: droid_pos = looked
        # print_area(area, droid_pos)
        if out1 == 2: oxygen = droid_pos

    print_area(area, (0, 0))
    print('# oxygen', oxygen)

    to_visit, visited = [((0, 0), 0)], []
    while to_visit:
        ((x, y), d) = to_visit.pop(0)
        visited.append((x, y))
        for m in [1, 4, 2, 3]:
            nxt = nextpos((x, y), m)
            val = area.get(nxt, None)
            if val == 2: print('XXX', d + 1); return # XXX
            if val != 1: continue
            if nxt in visited: continue
            to_visit.append(((*nxt,), d + 1))
        pass

#     ### ####### ################### ####### 
#    #...#.......#...................#.......#
#    #.#.#.###.#.#.#######.#########.#######.#
#    #.#...#.#.#.#.#...#...#.......#...#.....#
#    #.#####.#.#.#.#.#.#.###.###### ##.#.###.#
#    #...#...#.#.#.#.#...#.........#...#.#...#
#     ##.#.###.#.###.#############.#.###.#.## 
#    #...#.#...#.#...#.......#.....#.....#.#.#
#    #.###.#.###.#.#.#.#####.#.###########.#.#
#    #.....#.#.....#.#.#...#.#...........#.#.#
#     ####.#.#######.#.###.#.#.#######.###.#.#
#    #.#...#...#...#.#.#...#.#.......#...#...#
#    #.#.#####.#.#.###.#.###.#######.###.###.#
#    #...#...#...#...#.#.#...#.....#.#.#...#.#
#    #.###.#########.#.#.#.###.#.###.#.###.#.#
#    #.....#.........#.#.#...#.#...#.#.#...#.#
#     ####.#.###.#####.#.###.## ##.#.#.#.###.#
#    #.#...#.#...#...#.#...#...#...#...#.....#
#    #.#.###.#####.#.#.###.###.#.#.###.###### 
#    #.#.#.#.......#...#...#...#.#.#...#.....#
#    #.#.#.#############.###.###.###.###.###.#
#    #.#.......#.........#D#.#.....#.#...#.#.#
#    #.#######.#.#######.#.#.###.#.#.#.###.#.#
#    #.#.....#...#...#...#.#...#.#...#...#.#.#
#    #.#.#.#######.###.###.###.#.#### ##.#.#.#
#    #...#.#.........#.#.#...#.#.....#...#...#
#    #.###.#.#######.#.#.###.#.#####.#.###.## 
#    #.#.#...#.#...#...#...#.#.....#.#.#.#...#
#    #.#.#####.#.#.#####.#.#.#.###.#.#.#.###.#
#    #.#.........#.....#.#.#.#.#.#.#...#.....#
#    #.###########.#.#.#.###.#.#.#.#####.#### 
#    #...........#.#.#.#.#...#...#...#.#.#...#
#     ##########.###.#.#.#.###### ##.#.#.#.## 
#    #.........#...#.#...#.#.....#...#...#...#
#    #.#######.###.#.#####.#.###.#.###.###.#.#
#    #...#.#.....#...#.....#.#.#.#.#...#X..#.#
#     ##.#.#.#######.#.#####.#.#.#.#.#######.#
#    #...#.#.#.....#.#.#.....#...#.#.#.....#.#
#    #.###.#.#.#.###.#.###.###.###.#.###.#.#.#
#    #.....#...#.....#.....#.......#.....#...#
#     ##### ### ##### ##### ####### ##### ### 

# 406
def d19152(input=None):
    input = input or open('inputs/1915.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))

    droid_pos, droid_dir = (0, 0), None
    area, oxygen = {droid_pos: 1}, None

    def nextpos(droid_pos, droid_dir):
        mov = {1: (0, -1), 2: (0, 1), 3: (-1, 0), 4: (1, 0)}[droid_dir]
        return (droid_pos[0] + mov[0], droid_pos[1] + mov[1])

    def move_generator(prev_dir):
        nonlocal droid_dir, droid_pos, area, move_gen
        for m in [1, 4, 2, 3]:
            nxt = nextpos(droid_pos, m)
            val = area.get(nxt, None)
            if val != None: continue # we already know it
            yield m
            if droid_pos != nxt: continue # wall
            yield from move_generator(m)
        if not prev_dir: return # top level generator
        back_dir = {1:2, 2:1, 3:4, 4:3}[prev_dir]
        yield back_dir

    def inputf(): # 1:N 2:S 3:W 4:E
        nonlocal droid_dir, droid_pos, area, move_gen
        droid_dir = next(move_gen)
        return droid_dir

    move_gen = move_generator(droid_dir)
    generator = processIC13(input, inputf, False)

    for n in itertools.count():
        try: out1 = next(generator)
        except StopIteration: break
        looked = nextpos(droid_pos, droid_dir)
        area[looked] = out1
        if out1 != 0: droid_pos = looked
        if out1 == 2: oxygen = droid_pos

    print_area(area, (0, 0))
    print('# oxygen', oxygen)

    to_visit, visited = [oxygen], []
    for n in itertools.count():
        to_visit_next = []
        while to_visit:
            cell = to_visit.pop()
            visited.append(cell)
            for m in [1, 4, 2, 3]:
                nxt = nextpos(cell, m)
                val = area.get(nxt, None)
                if val != 1: continue
                if nxt in visited: continue
                to_visit_next.append(nxt)
        to_visit = list(to_visit_next)
        if not to_visit: break

    cells = list(
        filter(lambda x: area.get(x, None) >= 1, area.keys()))
    print('n', n, len(visited), len(cells))


# ###########################################################################
#
# 2019 DAY 14
#
# ###########################################################################

input141 = """
10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
"""

input142 = """
9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL
"""

input143 = """
157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
"""

input144 = """
2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF
"""

input145 = """
171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX
"""

# 522031
def d19141(input=None):
    input = input or open('inputs/1914.in').read()
    input = input.strip().splitlines()
    input = map(lambda line: re.findall('[0-9A-Z]+', line.strip()), input)
    prodf = {}
    for reac in input:
        reac = itertools.zip_longest(*[iter(reac)] * 2)
        reac = map(lambda n_e: (n_e[1], int(n_e[0])), reac)
        *comps, (res_elt, res_nb) = reac
        assert res_elt not in prodf
        prodf[res_elt] = (res_nb, comps)

    needed, extra = {'FUEL' : 1}, {}
    while True:
        needed_elts = list(set(needed.keys()) - set(['ORE']))
        if not needed_elts: break

        needed_elt, needed_nb = needed_elts[0], needed[needed_elts[0]]
        del needed[needed_elt]

        (prod_nb, prod_comps) = prodf[needed_elt]
        nb_reacs = 1 + ((needed_nb - 1) // prod_nb)

        for (comp_elt, comp_nb) in prod_comps:
            needed[comp_elt] = needed.get(comp_elt, 0) + comp_nb * nb_reacs
        extra[needed_elt] = extra.get(needed_elt, 0) + (nb_reacs * prod_nb - needed_nb)

        for (needed_elt, needed_nb) in needed.items():
            extra_nb = extra.get(needed_elt, 0)
            if not extra_nb: continue
            from_extra = min(needed_nb, extra_nb)
            extra[needed_elt] -= from_extra
            needed[needed_elt] -= from_extra

    print('# NB ORE', needed['ORE'])

# 3566577
def d19142(input=None):
    input = input or open('inputs/1914.in').read()
    input = input.strip().splitlines()
    input = map(lambda line: re.findall('[0-9A-Z]+', line.strip()), input)
    prodf = {}
    for reac in input:
        reac = itertools.zip_longest(*[iter(reac)] * 2)
        reac = map(lambda n_e: (n_e[1], int(n_e[0])), reac)
        *comps, (res_elt, res_nb) = reac
        assert res_elt not in prodf
        prodf[res_elt] = (res_nb, comps)

    range_start, range_end = 0, 10000000000
    while True:
        mid = range_start + (range_end - range_start) // 2

        needed, extra = {'FUEL': mid}, {}
        while True:
            needed_elts = list(set(needed.keys()) - set(['ORE']))
            if not needed_elts: break

            needed_elt, needed_nb = needed_elts[0], needed[needed_elts[0]]
            del needed[needed_elt]

            (prod_nb, prod_comps) = prodf[needed_elt]
            nb_reacs = 1 + ((needed_nb - 1) // prod_nb)

            for (comp_elt, comp_nb) in prod_comps:
                needed[comp_elt] = needed.get(comp_elt, 0) + comp_nb * nb_reacs
            extra[needed_elt] = extra.get(needed_elt, 0) + (nb_reacs * prod_nb - needed_nb)

            for (needed_elt, needed_nb) in needed.items():
                extra_nb = extra.get(needed_elt, 0)
                if not extra_nb: continue
                from_extra = min(needed_nb, extra_nb)
                extra[needed_elt] -= from_extra
                needed[needed_elt] -= from_extra

        if range_end <= range_start + 1:
            break
        if needed['ORE'] > 1000000000000:
            range_end = mid
        else: range_start = mid

    print('FUEL', range_start)


# ###########################################################################
#
# 2019 DAY 13
#
# ###########################################################################

def processIC13(prog, inputf, dbg=True):

    def debug(*args):
        if not dbg: return
        print(*args)

    def param(pm, op):
        if pm == 0: return prog[op]
        elif pm == 1: return op
        elif pm == 2: return prog[rb + op]
        else: assert 0

    def paddr(pm, op):
        if pm == 0: return op
        elif pm == 2: return rb + op
        else: assert 0

    def pstr(pm, op):
        if pm == 0: return 'M[%d](%d)' % (op, prog[op])
        elif pm == 1: return 'I(%d)' % (op)
        elif pm == 2: return 'M[%d+%d](%d)' % (rb, op, prog[rb + op])
        else: assert 0

    pc, rb = 0, 0

    while True:
        pm3, pm2, pm1, *opc = ('%05d' % prog[pc])
        pm3, pm2, pm1 = int(pm3), int(pm2), int(pm1)
        opc = int(''.join(opc))

        if opc == 1: # ADD
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | ADD | %s = %s + %s (= %d)' % (
                pc, prog[pc], pstr(pm3, op3), pstr(pm1, op1), pstr(pm2, op2),
                param(pm1, op1) + param(pm2, op2)))
            prog[paddr(pm3, op3)] = param(pm1, op1) + param(pm2, op2)
            pc += 4

        elif opc == 2: # MUL
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | MUL | %s = %s * %s (= %d)' % (
                pc, prog[pc], pstr(pm3, op3), pstr(pm1, op1), pstr(pm2, op2),
                param(pm1, op1) * param(pm2, op2)))
            prog[paddr(pm3, op3)] = param(pm1, op1) * param(pm2, op2)
            pc += 4

        elif opc == 3: # INPUT
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 2]
            input_ = inputf()
            debug('# PC=%05d OPC=%05d | INP | %s = (%s)' % (
                pc, prog[pc], pstr(pm1, op1), input_))
            prog[paddr(pm1, op1)] = input_ # = input.pop(0)
            pc += 2

        elif opc == 4: # OUTPUT
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 1, 2]
            output = param(pm1, op1)
            debug('# PC=%05d OPC=%05d | OUT | %s >>> %d' % (
                pc, prog[pc], pstr(pm1, op1), output))
            yield output
            pc += 2

        elif opc == 5: # JUMP_IF_TRUE
            op1, op2 = prog[pc + 1], prog[pc + 2]
            assert pm3 == 0 and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | JTR | if %s: goto %s' % (
                pc, prog[pc], pstr(pm1, op1), pstr(pm2, op2)))
            pc = param(pm2, op2) if param(pm1, op1) else (pc + 3)

        elif opc == 6: # JUMP_IF_FALSE
            op1, op2 = prog[pc + 1], prog[pc + 2]
            assert pm3 == 0 and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | JFL | if not %s: goto %s' % (
                pc, prog[pc], pstr(pm1, op1), pstr(pm2, op2)))
            pc = param(pm2, op2) if not param(pm1, op1) else (pc + 3)

        elif opc == 7: # LESS_THAN
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | LTH | %s = %s < %s (= %d)' % (
                pc, prog[pc], pstr(pm3, op3), pstr(pm1, op1), pstr(pm2, op2),
                1 if (param(pm1, op1) < param(pm2, op2)) else 0))
            prog[paddr(pm3, op3)] = 1 if (param(pm1, op1) < param(pm2, op2)) else 0
            pc += 4

        elif opc == 8: # EQUAL
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | EQU | %s = %s == %s (= %d)' % (
                pc, prog[pc], pstr(pm3, op3), pstr(pm1, op1), pstr(pm2, op2),
                1 if (param(pm1, op1) == param(pm2, op2)) else 0))
            prog[paddr(pm3, op3)] = 1 if (param(pm1, op1) == param(pm2, op2)) else 0
            pc += 4

        elif opc == 9: # ADJUST_RB
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 1, 2]
            debug('# PC=%05d OPC=%05d | NRB | RB = %s + rb(%d) = %d' % (
                pc, prog[pc], pstr(pm1, op1), rb, rb + param(pm1, op1)))
            rb += param(pm1, op1)
            pc += 2

        elif opc == 99: # EXIT
            debug('# PC=%05d OPC=%05d | EXT' % (pc, opc))
            break

        else:
            debug('# PC=%05d OPC=%05d | ???' % (pc, opc))
            assert 0
    return

# 173
def d19131(input=None):
    input = input or open('inputs/1913.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    tiles = {}
    generator = processIC9(input, [])
    for n in itertools.count():
        try: out1 = next(generator)
        except StopIteration: break
        out2 = next(generator)
        out3 = next(generator)
        tiles[(out1, out2)] = out3
    print('# NBLOCKS', len(list(filter(lambda x: x == 2, tiles.values()))))

def print_screen(tiles):
    max_x = max(map(operator.itemgetter(0), tiles.keys()))
    max_y = max(map(operator.itemgetter(1), tiles.keys()))
    for y in range(max_y + 1):
        for x in range(max_x + 1):
            v = tiles.get((x, y), 0)
            if v == 0: print(' ', end='')   # empty
            elif v == 1: print('#', end='') # wall
            elif v == 2: print('0', end='') # block
            elif v == 3: print('_', end='') # paddle
            elif v == 4: print('o', end='') # ball
        print()
    print()

# 8942
def d19132(input=None):
    input = input or open('inputs/1913.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    input[0] = 2 # insert 2 quarters
    for c in range(1360, 1397): input[c] = 3 # set some extra paddles :-)
    def inputf(): return 0 # do not move anything
        # while True:
        #     try: return int(builtins.input('in ? ') or 0)
        #     except ValueError: pass
    generator = processIC13(input, inputf, dbg=False)
    tiles = {}
    for n in itertools.count():
        try: out1 = next(generator)
        except StopIteration: break
        out2 = next(generator)
        out3 = next(generator)
        tiles[(out1, out2)] = out3
        print_screen(tiles)
    print('score =' , tiles[(-1, 0)])

def d19133(input=None):
    input = input or open('inputs/1913.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    input[0] = 2 # insert 2 quarters
    for c in range(1360, 1397): input[c] = 3 # set some extra paddles :-)
    def inputf(): return 0 # do not move anything
    generator = processIC13(input, inputf, dbg=False)
    #
    nblines = 20
    nl = int(subprocess.run(['tput', 'lines'], capture_output=True).stdout)
    yoffset = nl - nblines
    print('\n' * nblines)
    #
    tiles = {}
    for n in itertools.count():
        try: out1 = next(generator)
        except StopIteration: break
        out2 = next(generator)
        out3 = next(generator)
        tiles[(out1, out2)] = out3
        if (out1, out2) == (-1, 0):
            continue
        if out3 == 0: v = ' '   # empty
        elif out3 == 1: v = '#' # wall
        elif out3 == 2: v = '#' # block
        elif out3 == 3: v = '_' # paddle
        elif out3 == 4: v = 'o' # ball
        else: v = chr(out3)     #'?'
        print('\033[%d;%dH\033[?25l%s' % (yoffset + out2, out1 + 1, v), end='')
        sys.stdout.flush()
        time.sleep(.001)
    print('\033[%d;%dH\033[?25h' % (nl - 1, 0), end='')
    print('score =' , tiles[(-1, 0)])


# ###########################################################################
#
# 2019 DAY 12
#
# ###########################################################################

input121 = """
<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>
"""

input122 = """
<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>
"""

# 7687
def d19121(input=None):
    input = input or open('inputs/1912.in').read()
    moons = list(filter(bool, map(lambda line: list(
        map(int, re.findall('[\-0-9]+', line.strip()))), input.splitlines())))
    nbdms = len(moons[0])
    velcs = [list([0] * nbdms) for _ in moons]
    #
    print('## moons', moons)
    for n in itertools.count(1):
        print('# step %d' % (n))
        # update velocity
        for (m1, m2) in itertools.combinations(range(4), 2):
            for i in range(nbdms):
                if moons[m1][i] < moons[m2][i]:
                    velcs[m1][i] += 1 ; velcs[m2][i] -= 1
                elif moons[m1][i] > moons[m2][i]:
                    velcs[m1][i] -= 1 ; velcs[m2][i] += 1
                else: pass
        print('## velcs', velcs)
        # update positions
        for m1 in range(len(moons)):
            for i in range(nbdms):
                moons[m1][i] += velcs[m1][i]
        print('## moons', moons)
        # total energy
        total_energy = 0
        for m1 in range(len(moons)):
            pot = sum(map(abs, moons[m1]))
            kin = sum(map(abs, velcs[m1]))
            total_energy += (pot * kin)
        print('## total', total_energy)
        #
        if n == 1000: break

# 334945516288044
def d19122(input=None):
    input = input or open('inputs/1912.in').read()
    moons = list(filter(bool, map(lambda line: list(
        map(int, re.findall('[\-0-9]+', line.strip()))), input.splitlines())))
    nbdms = len(moons[0])
    velcs = [list([0] * nbdms) for _ in moons]
    #
    cycll = [0 for _ in range(nbdms)]
    initp = dict([(d, tuple([moons[m][d] for m in range(len(moons))])) for d in range(nbdms)])
    #
    # print('## moons', moons)
    for n in itertools.count(1):
        # print('# step %d' % (n))
        # update velocity
        for (m1, m2) in itertools.combinations(range(4), 2):
            for i in range(nbdms):
                if moons[m1][i] < moons[m2][i]:
                    velcs[m1][i] += 1 ; velcs[m2][i] -= 1
                elif moons[m1][i] > moons[m2][i]:
                    velcs[m1][i] -= 1 ; velcs[m2][i] += 1
                else: pass
        # print('## velcs', velcs)
        # update positions
        for m1 in range(len(moons)):
            for i in range(nbdms):
                moons[m1][i] += velcs[m1][i]
        # print('## moons', moons)
        #
        # if n == 3000: break

        for d1 in range(nbdms):
            if cycll[d1]: continue
            t2 = [velcs[m][d1] for m in range(len(moons))]
            if any(t2): continue
            t1 = tuple([moons[m][d1] for m in range(len(moons))])
            if t1 != initp[d1]: continue
            cycll[d1] = n
        if all(cycll): break
    def lcm(a, b):
        return abs(a * b) // math.gcd(a, b)
    print(lcm(lcm(cycll[0], cycll[1]), cycll[2]))


# ###########################################################################
#
# 2019 DAY 11
#
# ###########################################################################

# 1785
def d19111(input=None):
    input = input or open('inputs/1911.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    input_grid = collections.defaultdict(int)
    robot_pos, robot_dir = (0, 0), 'U'
    input_list = []
    generator = processIC9(input, input_list)
    for n in itertools.count():
        print('##### ROUND %d #####' % n)
        # fill new input
        input_list.append(input_grid[robot_pos])
        # color
        try: out1 = next(generator)
        except StopIteration: break
        input_grid[robot_pos] = out1 # 0: black, 1: white
        # movement
        out2 = next(generator)
        robot_dir = {'U': "LR", 'D': "RL", 'R': "UD", 'L': "DU"}[robot_dir][out2]
        robot_mov = {'U': (0, -1), 'D': (0, 1), 'R': (1, 0), 'L': (-1, 0)}[robot_dir]
        robot_pos = (robot_pos[0] + robot_mov[0], robot_pos[1] + robot_mov[1])

    print(len(input_grid.keys()))

# HJALJZFH
def d19112(input=None):
    input = input or open('inputs/1911.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    input_grid = collections.defaultdict(int)
    robot_pos, robot_dir = (0, 0), 'U'
    input_list = []
    generator = processIC9(input, input_list)
    input_grid[robot_pos] = 1
    for n in itertools.count():
        print('##### ROUND %d #####' % n)
        # fill new input
        input_list.append(input_grid[robot_pos])
        # color
        try: out1 = next(generator)
        except StopIteration: break
        input_grid[robot_pos] = out1 # 0: black, 1: white
        # movement
        out2 = next(generator)
        robot_dir = {'U': "LR", 'D': "RL", 'R': "UD", 'L': "DU"}[robot_dir][out2]
        robot_mov = {'U': (0, -1), 'D': (0, 1), 'R': (1, 0), 'L': (-1, 0)}[robot_dir]
        robot_pos = (robot_pos[0] + robot_mov[0], robot_pos[1] + robot_mov[1])
    max_xs = max(map(operator.itemgetter(0), input_grid.keys()))
    max_ys = max(map(operator.itemgetter(1), input_grid.keys()))
    for y in range(max_ys + 1):
        for x in range(max_xs + 1):
            print(' ' if not input_grid[(x, y)] else '#', end='')
        print()

    #  #   ##  ##  #      ## #### #### #  #
    #  #    # #  # #       #    # #    #  #
    ####    # #  # #       #   #  ###  ####
    #  #    # #### #       #  #   #    #  #
    #  # #  # #  # #    #  # #    #    #  #
    #  #  ##  #  # ####  ##  #### #    #  #


# ###########################################################################
#
# 2019 DAY 10
#
# ###########################################################################

input105 = """
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
""" # (11, 13) -> 210

# 263 [(23, 29)]
def d19101(input=None):
    input = input or open('inputs/1910.in').read()
    aslst = list(
        filter(bool, map(lambda l: l.strip(), input.splitlines())))

    def isvisible(x1, y1, x2, y2):
        if (x1, y1) == (x2, y2): return False
        (dx, dy) = (x2 - x1), (y2 - y1)
        (nx, ny) = (dx // math.gcd(dx, dy), dy // math.gcd(dx, dy))
        for f in range(1, (dx // nx) if nx else (dy // ny)):
            if (x1 + nx * f, y1 + ny * f) in astrs: return False
        return True

    astrs = set([])
    for (y, row) in enumerate(aslst):
        for (x, val) in enumerate(row):
            if val == '#': astrs.add((x, y))

    results = {}
    for (x1, y1) in astrs:
        nbvisible = len(list(filter(lambda a2: isvisible (x1, y1, a2[0], a2[1]), astrs)))
        results.setdefault(nbvisible, []).append((x1, y1))
    maxr = max(results.keys())
    print(maxr, results[maxr])
    return results[maxr][0]

def d19102(input=None):
    input_ = input or open('inputs/1910.in').read()
    aslst = list(
        filter(bool, map(lambda l: l.strip(), input_.splitlines())))

    astrs = set([])
    for (y, row) in enumerate(aslst):
        for (x, val) in enumerate(row):
            if val == '#': astrs.add((x, y))

    (x1, y1) = d19101(input)
    print('pos', (x1, y1))

    astrs = astrs - set([(x1, y1)])

    srast = {}
    for (x2, y2) in sorted(astrs):
        (dx, dy) = (x2 - x1), (y2 - y1)
        (nx, ny) = (dx // math.gcd(dx, dy), dy // math.gcd(dx, dy))
        angle = math.fmod(math.atan2(ny, nx) + (5.0 * math.pi / 2.0), math.pi * 2.0)
        dist = math.sqrt(dx ** 2 + dy ** 2)
        srast.setdefault(angle, []).append((dist, (x2, y2)))

    srast2 = []
    for (angle, alist) in srast.items():
        srast2.extend(list(map(
            lambda p_x: (p_x[0], angle, p_x[1][1]), enumerate(sorted(alist)))))
    srast2.sort()
    srast2 = list(map(lambda p_a_p: p_a_p[2], srast2))

    print(srast2[199][0] * 100 + srast2[199][1])


# ###########################################################################
#
# 2019 DAY 9
#
# ###########################################################################

def processIC9(prog, input):

    def param(pm, op):
        if pm == 0: return prog[op]
        elif pm == 1: return op
        elif pm == 2: return prog[rb + op]
        else: assert 0

    def paddr(pm, op):
        if pm == 0: return op
        elif pm == 2: return rb + op
        else: assert 0

    pc, rb = 0, 0

    while True:
        print('# PC=%05d OPC=%05d' % (pc, prog[pc]))
        pm3, pm2, pm1, *opc = ('%05d' % prog[pc])
        pm3, pm2, pm1 = int(pm3), int(pm2), int(pm1)
        opc = int(''.join(opc))
        print('# # # PM3=%d PM2=%d PM1=%d OPC=%02d' % (pm3, pm2, pm1, opc))

        if opc == 1: # ADD
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            prog[paddr(pm3, op3)] = param(pm1, op1) + param(pm2, op2)
            pc += 4

        elif opc == 2: # MUL
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            prog[paddr(pm3, op3)] = param(pm1, op1) * param(pm2, op2)
            pc += 4

        elif opc == 3: # INPUT
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 2]
            prog[paddr(pm1, op1)] = input_ = input.pop(0)
            print('<', input_)
            pc += 2

        elif opc == 4: # OUTPUT
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 1, 2]
            output = param(pm1, op1)
            print('> %d' % (output))
            yield output
            pc += 2

        elif opc == 5: # JUMP_IF_TRUE
            op1, op2 = prog[pc + 1], prog[pc + 2]
            assert pm3 == 0 and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            pc = param(pm2, op2) if param(pm1, op1) else (pc + 3)

        elif opc == 6: # JUMP_IF_FALSE
            op1, op2 = prog[pc + 1], prog[pc + 2]
            assert pm3 == 0 and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            pc = param(pm2, op2) if not param(pm1, op1) else (pc + 3)

        elif opc == 7: # LESS_THAN
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            prog[paddr(pm3, op3)] = 1 if (param(pm1, op1) < param(pm2, op2)) else 0
            pc += 4

        elif opc == 8: # EQUAL
            op1, op2, op3 = prog[pc + 1], prog[pc + 2], prog[pc + 3]
            assert pm3 in [0, 2] and pm2 in [0, 1, 2] and pm1 in [0, 1, 2]
            prog[paddr(pm3, op3)] = 1 if (param(pm1, op1) == param(pm2, op2)) else 0
            pc += 4

        elif opc == 9: # ADJUST_RB
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 1, 2]
            rb += param(pm1, op1)
            pc += 2

        elif prog[pc] == 99: # EXIT
            break

        else: assert 0
    return

# 2890527621
def d1991(input=None):
    input = input or open('inputs/1909.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    for x in processIC9(input, [1]): pass

# 66772
def d1992(input=None):
    input = input or open('inputs/1909.in').read()
    input = list(map(int, input.strip().split(',')))
    input = collections.defaultdict(int, enumerate(input))
    for x in processIC9(input, [2]): pass


# ###########################################################################
#
# 2019 DAY 8
#
# ###########################################################################

# 2460
def d1981(input=None):
    input = input or open('inputs/1908.in').read()
    input = [ c for c in input.strip()]
    layers = itertools.zip_longest(*([iter(input)] * 150), fillvalue=None)
    sortlr = sorted(map(collections.Counter, layers), key=operator.itemgetter('0'))
    print(sortlr[0]['1'] * sortlr[0]['2'])

# LRFKU
def d1982(input=None):
    szx, szy = 25, 6
    input = input or open('inputs/1908.in').read()
    input = [ c for c in input.strip()]
    layers = itertools.zip_longest(*([iter(input)] * (szx * szy)), fillvalue=None)
    pixels = map(lambda p: next(
        itertools.dropwhile(lambda x: x == '2', p)), zip(*layers))
    pxrows = itertools.zip_longest(*([iter(pixels)] * szx), fillvalue=None)
    print('\n'.join(map(lambda row: ''.join(
        map(lambda p: ' ' if p == '0' else 'X', row)), pxrows)))


# ###########################################################################
#
# 2019 DAY 7
#
# ###########################################################################

def processIC7(prog):
    pc= 0
    while True:
        print('# PC=%05d OPC=%05d' % (pc, prog[pc]))
        pm3, pm2, pm1, *opc = ('%05d' % prog[pc])
        pm3, pm2, pm1 = int(pm3), int(pm2), int(pm1)
        opc = int(''.join(opc))
        print('# # # PM3=%d PM2=%d PM1=%d OPC=%02d' % (pm3, pm2, pm1, opc))
        #
        if opc == 1: # ADD
            op1, op2, op3 = prog[pc + 1 : pc + 4]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            prog[op3] = (
                (op1 if (pm1 == 1) else prog[op1])
                +
                (op2 if (pm2 == 1) else prog[op2])
            )
            pc += 4
        elif opc == 2: # MUL
            op1, op2, op3 = prog[pc + 1 : pc + 4]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            prog[op3] = (
                (op1 if (pm1 == 1) else prog[op1])
                *
                (op2 if (pm2 == 1) else prog[op2])
            )
            pc += 4
        elif opc == 3: # INPUT
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 == 0
            print('# IN...')
            input = yield None
            print('<', input)
            prog[op1] = input
            pc += 2
        elif opc == 4: # OUTPUT
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 1]
            output = (op1 if (pm1 == 1) else prog[op1])
            print('> %d' % (output))
            yield output
            pc += 2
        elif opc == 5: # JUMP_IF_TRUE
            op1, op2 = prog[pc + 1 : pc + 3]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            op1val = (op1 if (pm1 == 1) else prog[op1])
            op2val = (op2 if (pm2 == 1) else prog[op2])
            pc = (
                op2val if (op1val) else (pc + 3)
            )
        elif opc == 6: # JUMP_IF_FALSE
            op1, op2 = prog[pc + 1 : pc + 3]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            op1val = (op1 if (pm1 == 1) else prog[op1])
            op2val = (op2 if (pm2 == 1) else prog[op2])
            pc = (
                op2val if (not op1val) else (pc + 3)
            )
        elif opc == 7: # LESS_THAN
            op1, op2, op3 = prog[pc + 1 : pc + 4]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            op1val = (op1 if (pm1 == 1) else prog[op1])
            op2val = (op2 if (pm2 == 1) else prog[op2])
            prog[op3] = (
                1 if (op1val < op2val) else 0
            )
            pc += 4
        elif opc == 8: # EQUAL
            op1, op2, op3 = prog[pc + 1 : pc + 4]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            op1val = (op1 if (pm1 == 1) else prog[op1])
            op2val = (op2 if (pm2 == 1) else prog[op2])
            prog[op3] = (
                1 if (op1val == op2val) else 0
            )
            pc += 4
        elif prog[pc] == 99:
            break
        else: assert 0
    return

# 368584
def d1971(input=None):
    input = input or open('inputs/1907.in').read()
    input = list(map(int, input.strip().split(',')))
    max_output = 0
    for sequence in itertools.permutations(range(5), 5):
        print('#### sequence', sequence)
        amp_input = 0
        for phase_setting in sequence:
            print('######## ', phase_setting, amp_input)
            amp_gen = processIC7(list(input))
            amp_gen.send(None)
            amp_gen.send(phase_setting)
            amp_input = amp_gen.send(amp_input)
        print(amp_input)
        max_output = max(max_output, amp_input)
    print('>', max_output)

# 35993240
def d1972(input=None):
    input = input or open('inputs/1907.in').read()
    input = list(map(int, input.strip().split(',')))
    max_output = 0
    for sequence in itertools.permutations(range(5, 10), 5):
        print('#### sequence', sequence)
        amp_genrs = []
        for (n, phase_setting) in enumerate(sequence):
            print('######## initialize', n, phase_setting)
            amp_gen = processIC7(list(input))
            amp_gen.send(None)
            amp_gen.send(phase_setting)
            amp_genrs.append(amp_gen)
        amp_input = 0
        for n in itertools.cycle(range(5)):
            print('######## next', n, amp_input)
            # waiting for input. give it and get the next output
            try: amp_input = amp_genrs[n].send(amp_input)
            # ended on previous iter: that is the end
            except StopIteration: break
            # exec until next input
            try: amp_genrs[n].send(None)
            # this generator ends now. others may still run
            except StopIteration: pass
        print('#### output', amp_input)
        max_output = max(max_output, amp_input)
    print('>', max_output)


# ###########################################################################
#
# 2019 DAY 6
#
# ###########################################################################

def lsorbs(orbits, d2root, obj):
    if obj in d2root:
        return d2root[obj]
    if obj in orbits:
        dobj = orbits[obj]
        res = [dobj] + lsorbs(orbits, d2root, dobj)
    else: res = []
    d2root[obj] = res
    return res

# 140608
def d1961(input=None):
    input = input or open('inputs/1906.in').read()
    orbits = dict(map(lambda l: reversed(l.split(')')), input.splitlines()))
    d2root = dict()
    for obj in orbits.keys(): lsorbs(orbits, d2root, obj)
    print(sum(map(len, d2root.values())))

# 337
def d1962(input=None):
    input = input or open('inputs/1906.in').read()
    orbits = dict(map(lambda l: reversed(l.split(')')), input.splitlines()))
    d2root = dict()
    for obj in orbits.keys(): lsorbs(orbits, d2root, obj)
    print(len(set(d2root['YOU']).symmetric_difference(set(d2root['SAN']))))


# ###########################################################################
#
# 2019 DAY 5
#
# ###########################################################################

def processIC5(prog, input):
    pc = 0
    while True:
        print('# PC=%05d OPC=%05d' % (pc, prog[pc]))
        pm3, pm2, pm1, *opc = ('%05d' % prog[pc])
        pm3, pm2, pm1 = int(pm3), int(pm2), int(pm1)
        opc = int(''.join(opc))
        print('# # # PM3=%d PM2=%d PM1=%d OPC=%02d' % (pm3, pm2, pm1, opc))
        #
        if opc == 1: # ADD
            op1, op2, op3 = prog[pc + 1 : pc + 4]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            prog[op3] = (
                (op1 if (pm1 == 1) else prog[op1])
                +
                (op2 if (pm2 == 1) else prog[op2])
            )
            pc += 4
        elif opc == 2: # MUL
            op1, op2, op3 = prog[pc + 1 : pc + 4]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            prog[op3] = (
                (op1 if (pm1 == 1) else prog[op1])
                *
                (op2 if (pm2 == 1) else prog[op2])
            )
            pc += 4
        elif opc == 3: # INPUT
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 == 0
            # inp = int(input('input ?'))
            prog[op1] = input
            pc += 2
        elif opc == 4: # OUTPUT
            op1 = prog[pc + 1]
            assert pm3 == 0 and pm2 == 0 and pm1 in [0, 1]
            out = (op1 if (pm1 == 1) else prog[op1])
            print('> %d' % (out))
            pc += 2
        elif opc == 5: # JUMP_IF_TRUE
            op1, op2 = prog[pc + 1 : pc + 3]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            op1val = (op1 if (pm1 == 1) else prog[op1])
            op2val = (op2 if (pm2 == 1) else prog[op2])
            pc = (
                op2val if (op1val) else (pc + 3)
            )
        elif opc == 6: # JUMP_IF_FALSE
            op1, op2 = prog[pc + 1 : pc + 3]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            op1val = (op1 if (pm1 == 1) else prog[op1])
            op2val = (op2 if (pm2 == 1) else prog[op2])
            pc = (
                op2val if (not op1val) else (pc + 3)
            )
        elif opc == 7: # LESS_THAN
            op1, op2, op3 = prog[pc + 1 : pc + 4]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            op1val = (op1 if (pm1 == 1) else prog[op1])
            op2val = (op2 if (pm2 == 1) else prog[op2])
            prog[op3] = (
                1 if (op1val < op2val) else 0
            )
            pc += 4
        elif opc == 8: # EQUAL
            op1, op2, op3 = prog[pc + 1 : pc + 4]
            assert pm3 == 0 and pm2 in [0, 1] and pm1 in [0, 1]
            op1val = (op1 if (pm1 == 1) else prog[op1])
            op2val = (op2 if (pm2 == 1) else prog[op2])
            prog[op3] = (
                1 if (op1val == op2val) else 0
            )
            pc += 4
        elif prog[pc] == 99:
            break
        else: assert 0
    return None

# 11193703
def d1951(input=None):
    input = input or open('inputs/1905.in').read()
    input = list(map(int, input.strip().split(',')))
    processIC5(input, 1)

# 12410607
def d1952(input=None):
    input = input or open('inputs/1905.in').read()
    input = list(map(int, input.strip().split(',')))
    processIC5(input, 5)


# ###########################################################################
#
# 2019 DAY 4
#
# ###########################################################################

# puzzle input: 367479-893698

def checkRuleG(n):
    strn = str(n)
    glen = map(lambda c_n: len(list(c_n[1])), itertools.groupby(strn))
    return max(glen) > 1

def checkRuleD(n):
    strn = str(n)
    for i in range(len(strn) - 1):
        if strn[i+1] < strn[i]:
            return False
    return True

def d1941(): # 495
    result = 0
    for n in range(367479, 893698 + 1):
        if checkRuleG(n) and checkRuleD(n):
            result += 1
    print(result)

def checkRuleG2(n):
    strn = str(n)
    glen = map(lambda c_n: len(list(c_n[1])), itertools.groupby(strn))
    return 2 in glen

def d1942(): # 305
    result = 0
    for n in range(367479, 893698 + 1):
        if checkRuleG2(n) and checkRuleD(n):
            result += 1
    print(result)


# ###########################################################################
#
# 2019 DAY 3
#
# ###########################################################################

def pathLocs(segs):
    x, y, pts = 0, 0, []
    for seg in segs:
        drc, lng = seg[0], int(seg[1:])
        if drc == 'U':
            pts.extend([(x, y - s) for s in range(1, lng + 1)])
        elif drc == 'D':
            pts.extend([(x, y + s) for s in range(1, lng + 1)])
        elif drc == 'R':
            pts.extend([(x + s, y) for s in range(1, lng + 1)])
        elif drc == 'L':
            pts.extend([(x - s, y) for s in range(1, lng + 1)])
        else: assert 0
        x, y = pts[-1]
    return pts

def mandist(x1, y1, x2, y2):
    return abs(x2 - x1) + abs(y2 - y1)

def d1931(): # 627
    input = map(
        lambda l: list(l.strip().split(',')),
        open('inputs/1903.in').readlines())
    path1, path2 = map(pathLocs, input)
    print(
        min(map(
            lambda crd: mandist(0, 0, *crd),
            set(path1) & set(path2)
        )))

def d1932(): # 13190
    input = map(
        lambda l: list(l.strip().split(',')),
        open('inputs/1903.in').readlines())
    path1, path2 = map(pathLocs, input)
    print(
        min(map(
            lambda i: path1.index(i) + path2.index(i) + 2,
            set(path1) & set(path2)
        )))


# ###########################################################################
#
# 2019 DAY 2
#
# ###########################################################################

def processIC(prog):
    pc = 0
    while True:
        if prog[pc] == 1:
            op1, op2, op3 = prog[pc + 1 : pc + 4]
            prog[op3] = prog[op1] + prog[op2]
            pc += 4
        elif prog[pc] == 2:
            op1, op2, op3 = prog[pc + 1 : pc + 4]
            prog[op3] = prog[op1] * prog[op2]
            pc += 4
        elif prog[pc] == 99:
            break
        else: assert 0
    return prog[0]

def d1921():
    input = list(
        map(int, open('inputs/1902.in').read().strip().split(',')))
    input[1 : 3] = (12, 2)
    print(processIC(input))

def d1922():
    input = list(
        map(int, open('inputs/1902.in').read().strip().split(',')))
    for (noun, verb) in [(x, y) for x in range(100) for y in range(100)]:
        prog = list(input)
        prog[1 : 3] = (noun, verb)
        if processIC(prog) == 19690720:
            print(100 * noun + verb)
            break


# ###########################################################################
#
# 2019 DAY 1
#
# ###########################################################################

def fuelReq(mass):
    return mass // 3 - 2;

def d1911():
    input = list(map(int, open('inputs/1901.in').readlines()))
    return sum(map(fuelReq, input))

def totalFuel(mass):
    total = 0
    while True:
        mass = fuelReq(mass)
        if mass <= 0: break
        total += mass
    return total

def d1912():
    input = list(map(int, open('inputs/1901.in').readlines()))
    return sum(map(totalFuel, input))

# ###########################################################################
