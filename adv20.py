#!/usr/bin/env python3

import os, sys, re, math, subprocess
import operator, itertools, functools, collections
import timeit, random, time, builtins

assert sys.hexversion >= 0x03080000

# exec(open('adv20.py').read())

# ###########################################################################

# ###########################################################################
#
# 2020 DAY 25
#
# ###########################################################################

# 18433997
def d20251(input = None):
    def find_loop_size(key, subject):
        value = 1
        for n in itertools.count(1):
            value *= subject
            value %= 20201227
            if value == key: break
        return n

    def transform_loop(subject, loop_size):
        value = 1
        for n in itertools.count(1):
            value *= subject
            value %= 20201227
            if n == loop_size: break
        return value

    # cards_key = 5764801
    # doors_key = 17807724

    # input = input or open('inputs/2025.in').read()
    # cards_key, doors_key = map(int, input.splitlines())

    cards_key = 18499292
    doors_key = 8790390

    cards_loop = find_loop_size(key=cards_key, subject=7) # 8
    doors_loop = find_loop_size(key=doors_key, subject=7) # 11

    print(cards_loop)
    print(doors_loop)

    print(transform_loop(doors_key, cards_loop))
    print(transform_loop(cards_key, doors_loop))


# ###########################################################################
#
# 2020 DAY 24
#
# ###########################################################################

# http://3dmdesign.com/development/hexmap-coordinates-the-easy-way
#
#  NW   NE
#    / \
# W |   | E  (X+1)
#    \ /
#  SW   SE
#   (Y+1) (X+1,Y+1)
#
def hexmove(s):
    x, y, i = 0, 0, 0
    while i < len(s):
        if s[i:i+1] == 'e':
            x += 1
            i += 1
        elif s[i:i+2] == 'se':
            x += 1; y += 1
            i += 2
        elif s[i:i+2] == 'sw':
            y += 1
            i += 2
        elif s[i:i+1] == 'w':
            x -= 1
            i += 1
        elif s[i:i+2] == 'nw':
            x -= 1; y -= 1
            i += 2
        elif s[i:i+2] == 'ne':
            y -= 1
            i += 2
        else: assert 0
    return (x, y)

# 293
def d20241(input = None):
    input = input or open('inputs/2024.in').read().splitlines()

    tiles = collections.defaultdict(int)

    for line in input:
        tiles[hexmove(line)] ^= 1

    print(sum(tiles.values()))

# 3967
def d20242(input = None):
    input = input or open('inputs/2024.in').read().splitlines()

    def neighs(x, y):
        return [
            (x+1, y),   (x-1, y),   # E  W
            (x+1, y+1), (x-1, y-1), # SE NW
            (x,   y+1), (x,   y-1)] # SW NE

    def nbactive(x, y):
        return sum(tiles[(nx, ny)] for (nx, ny) in neighs(x, y))

    tiles = collections.defaultdict(int)

    for line in input:
        tiles[hexmove(line)] ^= 1

    for n in itertools.count(1):
        ntiles = collections.defaultdict(int)
        xs, ys = list(zip(*tiles.keys()))
        for (x, y) in (
                (x, y)
                for x in range(min(xs) - 1, max(xs) + 2)
                for y in range(min(ys) - 1, max(ys) + 2)):
            nb = nbactive(x, y)
            if tiles[(x, y)]:
                ntiles[x, y] = 0 if (nb == 0 or nb > 2) else 1
            else:
                ntiles[x, y] = 1 if (nb == 2) else 0

        tiles = ntiles

        print(n, sum(tiles.values()))

        if n  == 100: break

    print(sum(tiles.values()))


# ###########################################################################
#
# 2020 DAY 23
#
# ###########################################################################

# 25468379
def d20231(input = None):
    input = input or open('inputs/2023.in').read().strip() # 193467258

    input = collections.deque(map(int, input), maxlen=len(input))
    nbrounds = 100
    max_value = max(input)

    for n in itertools.count(1):
        current = input.popleft()
        input.appendleft(current)

        # pick up the three cups
        input.rotate(-1)
        c1 = input.popleft()
        c2 = input.popleft()
        c3 = input.popleft()
        input.rotate(+1)

        # select a destination cup
        destination = current
        while True:
            destination = destination - 1
            if destination == 0: destination = max_value
            if not destination in [c1, c2, c3]: break
        destination_index = input.index(destination) # long

        # insert cups
        input.rotate(-destination_index-1)
        input.extendleft([c3, c2, c1])
        input.rotate(destination_index)

        if n == nbrounds: break

    input.rotate(-input.index(1))
    result = ''.join((map(str, list(input)[1:])))
    print(f'XXX {n=} {result=}')

# 474747880250
def d20232(input = None):
    input = input or open('inputs/2023.in').read().strip() # 193467258

    max_value = 1_000_000 # 9
    input = list(map(int, input))
    input += list(range(max(input) + 1, max_value + 1))
    nbrounds = 10_000_000

    indct = dict(
        zip(input, itertools.islice(itertools.cycle(input), 1, None) ))

    current = input[0]

    for n in itertools.count(1):

        # pick up the three cups
        c1 = indct[current]
        c2 = indct[c1]
        c3 = indct[c2]
        indct[current] = indct[c3]

        # select a destination cup
        destination = current
        while True:
            destination = destination - 1
            if destination == 0: destination = max_value
            if not destination in [c1, c2, c3]: break

        # insert cups
        prev_dest_next = indct[destination]
        indct[destination] = c1
        indct[c3] = prev_dest_next

        # select new current
        current = indct[current]

        if n == nbrounds: break

    print(indct[1] * indct[indct[1]])


# ###########################################################################
#
# 2020 DAY 22
#
# ###########################################################################

# 30138
def d20221():
    input = open('inputs/2022.in').read()
    p1, p2 = list(map(lambda x: list(map(
        int, x.splitlines()[1:])), input.split('\n\n')))

    while p1 and p2:
        f1, f2 = p1.pop(0), p2.pop(0)
        if f1 > f2:
            p1.extend([f1, f2])
        else:
            p2.extend([f2, f1])

    winner = p1 or p2

    print(sum(map(lambda n_v: (n_v[0] + 1) * n_v[1], enumerate(reversed(winner)))))

# 31587
def d20222():
    input = open('inputs/2022.in').read()
    p1, p2 = list(map(lambda x: list(map(
        int, x.splitlines()[1:])), input.split('\n\n')))

    def recgame(p1, p2):
        print(f'# new game {p1=} {p2=} ')

        seen = set([])
        while p1 and p2:
            roundstr = f'{p1=} {p2=}'
            print('#    ### new round', roundstr)

            if roundstr in seen:
                print('# ### -->> already seen')
                return True # XXX ?
            seen.add(roundstr)

            f1, f2 = p1.pop(0), p2.pop(0)
            if len(p1) >= f1 and len(p2) >= f2:
                w1 = recgame(p1[:f1], p2[:f2])
            else:
                w1 = f1 > f2

            if w1:
                p1.extend([f1, f2])
            else:
                p2.extend([f2, f1])

        score = sum(map(lambda n_v: (n_v[0] + 1) * n_v[1], enumerate(reversed(p1 or p2))))
        print(f'# --> end game {p1=} {p2=} --> {score=}')

        return bool(p1)

    recgame(p1, p2)


# ###########################################################################
#
# 2020 DAY 21
#
# ###########################################################################

# mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
# trh fvjkl sbzzf mxmxvkd (contains dairy)
# sqjhc fvjkl (contains soy)
# sqjhc mxmxvkd sbzzf (contains fish)

# 2020
def d20211():
    input = open('inputs/2021.in').read()
    input = list(tuple(
        map(str.split, re.sub('\W', ' ', line).split('contains'))
    ) for line in input.splitlines())

    ingreds = list(set(sum(map(operator.itemgetter(0), input), [])))
    allergs = list(set(sum(map(operator.itemgetter(1), input), [])))
    print('ING', ingreds)
    print('ALG', allergs)

    poss = dict((alg, set(ingreds)) for alg in allergs)
    for (ings, algs) in input:
        for alg in algs: poss[alg] &= set(ings)
    print('POS0', poss)

    done = set({})
    while True:
        alg, ings = list(
            filter(lambda k_v: (k_v[0] not in done) and (len(k_v[1]) == 1), poss.items()))[0]
        for kalg in (poss.keys() - set([alg])): poss[kalg] -= ings
        done.add(alg)
        if done == set(allergs): break
    print('POS1', poss)

    not_allergs = set(ingreds) - set([]).union(*list(poss.values()))

    print(sum(len(set(ings).intersection(not_allergs)) for (ings, algs) in input))

# bcdgf,xhrdsl,vndrb,dhbxtb,lbnmsr,scxxn,bvcrrfbr,xcgtv
def d20212():
    input = open('inputs/2021.in').read()
    input = list(tuple(
        map(str.split, re.sub('\W', ' ', line).split('contains'))
    ) for line in input.splitlines())

    ingreds = list(set(sum(map(operator.itemgetter(0), input), [])))
    allergs = list(set(sum(map(operator.itemgetter(1), input), [])))
    print('ING', ingreds)
    print('ALG', allergs)

    poss = dict((alg, set(ingreds)) for alg in allergs)
    for (ings, algs) in input:
        for alg in algs: poss[alg] &= set(ings)
    print('POS0', poss)

    done = set({})
    while True:
        alg, ings = list(
            filter(lambda k_v: (k_v[0] not in done) and (len(k_v[1]) == 1), poss.items()))[0]
        for kalg in (poss.keys() - set([alg])): poss[kalg] -= ings
        done.add(alg)
        if done == set(allergs): break
    print('POS1', poss)

    print(','.join(
        (ings.pop()) for (alg, ings) in sorted(poss.items(), key=operator.itemgetter(0))))


# ###########################################################################
#
# 2020 DAY 20
#
# ###########################################################################

# #...##.#.. ..###..### #.#.#####.
# ..#.#..#.# ###...#.#. .#..######
# .###....#. ..#....#.. ..#.......
# ###.##.##. .#.#.#..## ######....
# .###.##### ##...#.### ####.#..#.
# .##.#....# ##.##.###. .#...#.##.
# #...###### ####.#...# #.#####.##
# .....#..## #...##..#. ..#.###...
# #.####...# ##..#..... ..#.......
# #.##...##. ..##.#..#. ..#.###...

# #.##...##. ..##.#..#. ..#.###...
# ##..#.##.. ..#..###.# ##.##....#
# ##.####... .#.####.#. ..#.###..#
# ####.#.#.. ...#.##### ###.#..###
# .#.####... ...##..##. .######.##
# .##..##.#. ....#...## #.#.#.#...
# ....#..#.# #.#.#.##.# #.###.###.
# ..#.#..... .#.##.#..# #.###.##..
# ####.#.... .#..#.##.. .######...
# ...#.#.#.# ###.##.#.. .##...####

# ...#.#.#.# ###.##.#.. .##...####
# ..#.#.###. ..##.##.## #..#.##..#
# ..####.### ##.#...##. .#.#..#.##
# #..#.#..#. ...#.#.#.. .####.###.
# .#..####.# #..#.#.#.# ####.###..
# .#####..## #####...#. .##....##.
# ##.##..#.. ..#...#... .####...#.
# #.#.###... .##..##... .####.##.#
# #...###... ..##...#.. ...#..####
# ..#.#....# ##.#.#.... ...##.....

# 17250897231301
def d20201():
    input = open('inputs/2020.in').read()
    grids = list(
        paragr.splitlines() for paragr in input.split('\n\n'))
    grids = dict(
       (int(first[5:-1]), grid) for (first, *grid) in grids)

    sides = {}
    for (gridnum, lines) in grids.items():
        sides.setdefault(gridnum, [])
        sides[gridnum].append(lines[0])
        sides[gridnum].append(lines[-1])
        sides[gridnum].append(''.join(line[0] for line in lines))
        sides[gridnum].append(''.join(line[-1] for line in lines))
        sides[gridnum].extend(
            ''.join(reversed(side)) for side in list(sides[gridnum]))

    poss = {}
    for ((g1, s1), (g2, s2)) in itertools.combinations(sides.items(), 2):
        if not len(set(s1) & set(s2)): continue
        poss[g1] = poss.setdefault(g1, 0) + 1
        poss[g2] = poss.setdefault(g2, 0) + 1
    corners = sorted(poss.items(), key=operator.itemgetter(1))[:4]

    print(list(map(operator.itemgetter(0), corners)))
    print(functools.reduce(operator.mul, map(operator.itemgetter(0), corners)))

# 1576
def d20202():
    input = open('inputs/2020.in').read()
    grids = list(
        paragr.splitlines() for paragr in input.split('\n\n'))
    grids = dict(
        (int(first[5:-1]), list(map(list, grid))) for (first, *grid) in grids)

    def sides(grid):
        yield from [
            grid[0],
            list(line[-1] for line in grid),
            grid[-1],
            list(line[0] for line in grid)]

    def variants(grid):
        yield grid
        for i in range(3):
            yield (grid := list(map(list, zip(*grid[::-1]))))
        yield (grid := list(grid[::-1]))
        for i in range(3):
            yield (grid := list(map(list, zip(*grid[::-1]))))

    big_grid, grid_pos = {}, {} # pos -> grid, grid -> pos
    pending, todo = [], list(grids.keys())

    pending.append(first := todo.pop())
    big_grid[(0, 0)] = grids[first]
    grid_pos[first] = (0, 0)

    while pending:
        grid = pending.pop()
        for (sidenum, side) in enumerate(sides(big_grid[grid_pos[grid]])):

            grid_x, grid_y = grid_pos[grid]
            grid_nx = grid_x + {0:0, 1:1, 2:0, 3:-1}[sidenum]
            grid_ny = grid_y + {0:-1, 1:0, 2:1, 3:0}[sidenum]
            if big_grid.get((grid_nx, grid_ny), None): continue

            for other in todo:
                for variant in variants(grids[other]):
                    variant_sides = list(sides(variant))
                    if variant_sides[(sidenum + 2) % 4] == side:
                        big_grid[(grid_nx, grid_ny)] = variant
                        grid_pos[other] = (grid_nx, grid_ny)
                        pending.append(other)
                        todo.remove(other)
                        break

                else: continue
                break

    image_lines = []
    nb_lines = len(big_grid[(0,0)]) - 2
    xs, ys = list(zip(*big_grid.keys()))
    for y in range(min(ys), max(ys) + 1):
        for x in range(min(xs), max(xs) + 1):
            for (n, line) in enumerate(big_grid[(x, y)][1:-1]):
                if x == min(xs): image_lines.append('')
                line = ''.join(line[1:-1])
                image_lines[(y - min(ys)) * nb_lines + n] += line
            pass
    print('\n'.join(image_lines))

    #  ....................
    # .                  #
    # .#    ##    ##    ###
    # . #  #  #  #  #  #

    monster = [
        (0, 18),
        (1, 0), (1, 5), (1, 6), (1, 11), (1, 12), (1, 17), (1, 18), (1, 19),
        (2, 1), (2, 4), (2, 7), (2, 10), (2, 13), (2, 16)]

    for variant in variants(image_lines):
        found = False
        for y in range(len(variant) - 1):
            for x in range(len(variant[0]) - 18):
                for (dy, dx) in monster:
                    if not variant[y + dy][x + dx] in ['#', 'O']: break
                else:
                    print(x, y)
                    for (dy, dx) in monster: variant[y + dy][x + dx] = 'O'
                    found = True
        if not found: continue
        print('\n'.join(map(lambda l: ''.join(l), variant)) + '\n')
        break

    print(len(list(filter(lambda c: c == '#', sum(variant, [])))))

    # .####...#####..#...###..
    # #####..#..#.#.####..#.#.
    # .#.#...#.###...#.##.O#..
    # #.O.##.OO#.#.OO.##.OOO##
    # ..#O.#O#.O##O..O.#O##.##
    # ...#.#..##.##...#..#..##
    # #.##.#..#.#..#..##.#.#..
    # .###.##.....#...###.#...
    # #.####.#.#....##.#..#.#.
    # ##...#..#....#..#...####
    # ..#.##...###..#.#####..#
    # ....#.##.#.#####....#...
    # ..##.##.###.....#.##..#.
    # #...#...###..####....##.
    # .#.##...#.##.#.#.###...#
    # #.###.#..####...##..#...
    # #.###...#.##...#.##O###.
    # .O##.#OO.###OO##..OOO##.
    # ..O#.O..O..O.#O##O##.###
    # #.#..##.########..#..##.
    # #.#####..#.#...##..#....
    # #....##..#.#########..##
    # #...#.....#..##...###.##
    # #..###....##.#...##.##.#


# ###########################################################################
#
# 2020 DAY 19
#
# ###########################################################################

def match2019(rules, str):
    def check(rulenum, endstr):
        rule = rules.get(rulenum)
        # simple poss with character
        if rule[0][0].startswith('"'):
            if not endstr.startswith(rule[0][0][1]):
                return set([])
            return set([endstr[1:]])
        # sequence of other rules
        all_matches = set()
        for poss in rule:
            poss_matches = set([endstr])
            for elem in poss:
                match_elem = set()
                for end in poss_matches:
                    match_elem |= check(int(elem), end)
                poss_matches = match_elem
            all_matches |= poss_matches
        return all_matches
    return '' in check(0, str)

# 109
def d20191():
    input = open('inputs/2019.in').read()
    rules, input = map(str.splitlines, input.split('\n\n'))
    rules = dict(itertools.starmap(lambda n, v: (int(n), list(
        e.strip().split() for e in v.split('|'))), map(lambda l: l.split(':'), rules)))

    print(len(list(filter(lambda i: match2019(rules, i), input))))

# 301
def d20192():
    input = open('inputs/2019.in').read()
    rules, input = map(str.splitlines, input.split('\n\n'))
    rules = dict(itertools.starmap(lambda n, v: (int(n), list(
        e.strip().split() for e in v.split('|'))), map(lambda l: l.split(':'), rules)))

    rules.update({8: [['42'], ['42', '8']], 11: [['42', '31'], ['42', '11', '31']]})

    print(len(list(filter(lambda i: match2019(rules, i), input))))


# ###########################################################################
#
# 2020 DAY 18
#
# ###########################################################################

# 14006719520523
def d20181(input = None):
    input = input or open('inputs/2018.in').read()
    input = input.splitlines()

    def evalx(expr):
        class advint(int):
            def __add__(self, b):
                return advint(int(self) + b)
            def __sub__(self, b):
                return advint(int(self) * b)
        expr = ''.join(map(lambda c: {'*':'-'}.get(c, c), expr))
        expr = re.sub('(\d+)', 'advint(\\1)', expr)
        return eval(expr)

    print(sum(evalx(line) for line in input))

# 545115449981968
def d20182(input = None):
    input = input or open('inputs/2018.in').read()
    input = input.splitlines()

    def evalx(expr):
        class advint(int):
            def __add__(self, b):
                return advint(int(self) * b)
            def __mul__(self, b):
                return advint(int(self) + b)
        expr = ''.join(map(lambda c: {'+':'*', '*':'+'}.get(c, c), expr))
        expr = re.sub('(\d+)', 'advint(\\1)', expr)
        return eval(expr)

    print(sum(evalx(line) for line in input))

# def d20181_re(input = None):
#     input = input or open('inputs/2018.in').read()
#     input = input.splitlines()
#
#     def evalx(expr):
#         while True:
#             while (re_obj := re.search('(^|\()((\d+) (\+|\*) (\d+))', expr)):
#                 expr = re.sub(re_obj.re, re_obj.group(1) + str(eval(re_obj.group(2))), expr, count=1)
#             while (re_obj := re.search('\((\d+)\)', expr)):
#                 expr = re.sub(re_obj.re, '\\1', expr, count=1)
#             if (re.match('^\d+$', expr)): break
#         return int(expr)
#
#     print(sum(evalx(line) for line in input))

# def d20181_pr(input = None):
#     input = input or open('inputs/2018.in').read()
#     input = input.splitlines()
#
#     class parser_():
#         def __init__(self, str):
#             self.lexer = (t.group(0) for t in re.finditer('(\(|\+|\*|\d+|\))', str))
#             self.token = next(self.lexer)
#
#         def check_token(self, x):
#             if isinstance(x, str):
#                 return self.token == x
#             else: # func for token type check
#                 return x(self.token)
#
#         def advance(self):
#             self.prev_token = self.token
#             try: self.token = next(self.lexer)
#             except StopIteration: pass
#
#         def accept(self, c):
#             if not self.check_token(c): return False
#             self.advance()
#             return True
#
#         def expect(self, c):
#             assert self.check_token(c)
#             self.advance()
#             return True
#
#     # expr -> term
#     # term -> fact "+" fact | fact
#     # fact -> prim "*" prim | prim
#     # prim -> NUMBER | "(" expr ")"
#
#     def evalx(s):
#
#         def parse_expr(parser):
#             expr1 = parse_fact(parser)
#             if parser.accept('+'):
#                 expr2 = parse_fact(parser)
#                 return expr1 + expr2
#             return expr1
#
#         def parse_fact(parser):
#             expr1 = parse_prim(parser)
#             if parser.accept('*'):
#                 expr2 = parse_prim(parser)
#                 return expr1 * expr2
#             return expr1
#
#         def parse_prim(parser):
#             if parser.accept('('):
#                 expr = parse_expr(parser)
#                 parser.expect(')')
#                 return expr
#             elif parser.accept(str.isnumeric):
#                 expr = int(parser.prev_token)
#                 return expr
#             else: assert 0
#
#         return parse_expr(parser_(s))
#
#     print(evalx(input[0]))


# ###########################################################################
#
# 2020 DAY 17
#
# ###########################################################################

input17 = """#.##.##.
.##..#..
....#..#
.##....#
#..##...
.###..#.
..#.#..#
.....#..
"""

input170 = """.#.\n..#\n###\n"""

# 273
def d20171(input = None):
    input = input or open('inputs/2017.in').read()

    grid = collections.defaultdict(int, (
        ((x, y, 0), 1)
        for (y, l) in enumerate(input.splitlines())
        for (x, c) in enumerate(l.strip()) if c == '#'))

    def neighs(x, y, z):
        return (
            (nx, ny, nz)
            for nx in range(x - 1, x + 2)
            for ny in range(y - 1, y + 2)
            for nz in range(z - 1, z + 2)
            if (nx, ny, nz) != (x, y, z))

    def nbactive(x, y, z):
        return sum(grid[(nx, ny, nz)] for (nx, ny, nz) in neighs(x, y, z))

    for n in range(6):
        ngrid = collections.defaultdict(int)
        xs, ys, zs = list(zip(*grid.keys()))
        for (x, y, z) in (
                (x, y, z)
                for x in range(min(xs) - 1, max(xs) + 2)
                for y in range(min(ys) - 1, max(ys) + 2)
                for z in range(min(zs) - 1, max(zs) + 2)):
            if grid[(x, y, z)]:
                ngrid[(x, y, z)] = (2 <= nbactive(x, y, z) <= 3) and 1 or 0
            else:
                ngrid[(x, y, z)] = (nbactive(x, y, z) == 3) and 1 or 0
        grid = ngrid

    print(sum(grid.values()))

# 1504
def d20172(input = None):
    input = input or open('inputs/2017.in').read()

    grid = collections.defaultdict(int, (
        ((x, y, 0, 0), 1)
        for (y, l) in enumerate(input.splitlines())
        for (x, c) in enumerate(l.strip()) if c == '#'))

    def neighs(x, y, z, w):
        return (
            (nx, ny, nz, nw)
            for nx in range(x - 1, x + 2)
            for ny in range(y - 1, y + 2)
            for nz in range(z - 1, z + 2)
            for nw in range(w - 1, w + 2)
            if (nx, ny, nz, nw) != (x, y, z, w))

    def nbactive(x, y, z, w):
        return sum(grid[(nx, ny, nz, nw)] for (nx, ny, nz, nw) in neighs(x, y, z, w))

    for n in range(6):
        ngrid = collections.defaultdict(int)
        xs, ys, zs, ws = list(zip(*grid.keys()))
        for (x, y, z, w) in (
                (x, y, z, w)
                for x in range(min(xs) - 1, max(xs) + 2)
                for y in range(min(ys) - 1, max(ys) + 2)
                for z in range(min(zs) - 1, max(zs) + 2)
                for w in range(min(ws) - 1, max(ws) + 2)):
            if grid[(x, y, z, w)]:
                ngrid[(x, y, z, w)] = (2 <= nbactive(x, y, z, w) <= 3) and 1 or 0
            else:
                ngrid[(x, y, z, w)] = (nbactive(x, y, z, w) == 3) and 1 or 0
        grid = ngrid

    print(sum(grid.values()))


# ###########################################################################
#
# 2020 DAY 16
#
# ###########################################################################

# 18142
def d20161():
    input = open('inputs/2016.in').read()
    par1, par2, par3 = input.split('\n\n')

    ranges = list(
        map(lambda t: list(map(int, t)), re.findall(
            '^.+: (\d+)-(\d+) or (\d+)-(\d+)', par1, re.MULTILINE)))
    mytckt = list(
        map(int, par2.splitlines()[1].split(',')))
    nearbt = list(
        map(lambda l: list(map(int, l.split(','))), par3.splitlines()[1:]))

    valids = set(sum(
        (list(range(a1, a2 + 1)) + list(range(b1, b2 + 1))
         for (a1, a2, b1, b2) in ranges), []))

    tserate = sum(sum(
        list(filter(bool, map(lambda l: list(set(l) - valids), nearbt))), []))
    print(tserate)

# 1069784384303
def d20162():
    input = open('inputs/2016.in').read()
    par1, par2, par3 = input.split('\n\n')

    ranges = list(
        map(lambda t: (t[0], list(map(int, t[1:]))), re.findall(
            '(^.+): (\d+)-(\d+) or (\d+)-(\d+)', par1, re.MULTILINE)))
    mytckt = list(
        map(int, par2.splitlines()[1].split(',')))
    nearbt = list(
        map(lambda l: list(map(int, l.split(','))), par3.splitlines()[1:]))

    valids = set(sum(
        (list(range(a1, a2 + 1)) + list(range(b1, b2 + 1))
         for (_, (a1, a2, b1, b2)) in ranges), []))

    nearbt = list(
        filter(lambda l: not len(set(l) - valids), nearbt))

    posspos = dict(
        (rid, set(range(max(map(len, nearbt)))).intersection(
            *((set(n for (n, v) in enumerate(nt)
                   if a1 <= v <= a2 or b1 <= v <= b2)) for nt in nearbt)))
        for (rid, (a1, a2, b1, b2)) in ranges)

    todo, done = posspos, {}
    while todo:
        todo = dict(sorted(todo.items(), key=lambda k_v: len(k_v[1])))
        (firstid, values) = list(todo.items())[0]
        assert len(values) == 1
        value = list(values)[0]
        done[firstid] = value
        del todo[firstid]
        todo = dict((k, set(filter(lambda x: x != value, v))) for (k, v) in todo.items())

    indexes = list(
        map(lambda k_v: k_v[1], filter(
            lambda k_v: k_v[0].startswith('departure'), done.items())))
    print(functools.reduce(operator.mul, (mytckt[i] for i in indexes), 1))


# ###########################################################################
#
# 2020 DAY 15
#
# ###########################################################################

# 2020 # [2, 1, 10, 11, 0, 6]
# 2020 # [0, 3, 6] # 436
# 2020 # [1, 3, 2] # 1
# 2020 # [2, 1, 3] # 10
# 2020 # [1, 2, 3] # 27
# 2020 # [2, 3, 1] # 78
# 2020 # [3, 2, 1] # 438
# 2020 # [3, 1, 2] # 1836

# 232
def d20151():
    input = list(map(int, open('inputs/2015.in').read().split(',')))
    # input = [2, 1, 10, 11, 0, 6]

    limit = 2020
    last_spoken = '?'
    idx = collections.defaultdict(int)

    for n in itertools.count(1):
        spoken = (
            (input.pop(0)) if input else
            (n - idx[last_spoken] - 1) if idx[last_spoken] else 0)
        idx[last_spoken] = n - 1
        last_spoken = spoken
        if n == limit: break

    print(spoken)

#18929178
def d20152():
    input = list(map(int, open('inputs/2015.in').read().split(',')))

    limit = 30000000
    last_spoken = '?'
    idx = collections.defaultdict(int)

    for n in itertools.count(1):
        spoken = (
            (input.pop(0)) if input else
            (n - idx[last_spoken] - 1) if idx[last_spoken] else 0)
        idx[last_spoken] = n - 1
        last_spoken = spoken
        if n == limit: break

    print(spoken)


# ###########################################################################
#
# 2020 DAY 14
#
# ###########################################################################

# 4886706177792
def d20141():
    input = open('inputs/2014.in').read().splitlines()
    memory = collections.defaultdict(int)
    for stmt in input:
        if stmt.startswith('mask'):
            bmask = stmt.split()[-1]
        elif stmt.startswith('mem'):
            re_obj = re.match('^mem\[(\d+)\] = (\d+)$', stmt)
            idx, val = list(map(int, re_obj.groups()))
            val = val | int(bmask.replace('X', '0'), 2)
            val = val & int(bmask.replace('X', '1'), 2)
            memory[idx] = val
        else: assert 0
    print(sum(memory.values()))

# 3348493585827
def d20142():
    input = open('inputs/2014.in').read().splitlines()
    memory = collections.defaultdict(int)
    for stmt in input:
        if stmt.startswith('mask'):
            bmask = stmt.split()[-1]
        elif stmt.startswith('mem'):
            re_obj = re.match('^mem\[(\d+)\] = (\d+)$', stmt)
            idx, val = list(map(int, re_obj.groups()))
            # XXXXXXXXXXXXXXX
            xis = list(map(lambda n_x: n_x[0], filter(
                lambda n_x: n_x[1] == 'X', enumerate(bmask.replace('1', '0')))))
            for xvs in itertools.product(['0','1'], repeat=len(xis)):
                nidx = idx | int(bmask.replace('X', '0'), 2)
                nidx = list(format(nidx, '036b'))
                for (xi, xv) in zip(xis, xvs):
                    nidx[xi] = xv
                nidx = int(''.join(nidx), 2)
                memory[nidx] = val
            # XXXXXXXXXXXXXXX
        else: assert 0
    print(sum(memory.values()))


# ###########################################################################
#
# 2020 DAY 13
#
# ###########################################################################

# 4207
def d20131():
    input = open('inputs/2013.in').read().splitlines()
    tms, ids = int(input[0]), list(map(int, filter(
        lambda x: x[0].isdigit(), input[1].split(','))))
    wait, busid = min(map(lambda x: (x * (1 + tms // x) - tms, x), ids))
    print(wait, busid, wait * busid)

# 725850285300475
def d20132():
    input = open('inputs/2013.in').read().splitlines()
    input = enumerate(int(x) if (x != 'x') else 0 for x in input[1].split(','))
    input = list(filter(lambda n_i: bool(n_i[1]), input))

    cur_time, cur_incr = 0, 1
    for (bus_offset, bus_id) in input:
        while (cur_time + bus_offset) % bus_id:
            cur_time += cur_incr
        cur_incr *= bus_id
    print(cur_time)


# ###########################################################################
#
# 2020 DAY 12
#
# ###########################################################################

def mandist(x1, y1, x2=0, y2=0):
    return abs(x2 - x1) + abs(y2 - y1)

# 1457
def d20121():
    input = open('inputs/2012.in').read()
    input = list(map(lambda l: (l[0], int(l[1:])), input.splitlines()))

    x, y, d = 0, 0, 0 # E0 S1 W2 N3
    for (c, n) in input:
        # N -> move north by the given value
        if c == 'N':
            y = y - n
        # S -> move south by the given value
        elif c == 'S':
            y = y + n
        # E -> move east by the given value
        elif c == 'E':
            x = x + n
        # W -> move west by the given value
        elif c == 'W':
            x = x - n
        # L -> turn left the given number of degrees
        elif c == 'L':
            d = (d - (n // 90)) % 4
        # R -> turn right the given number of degrees
        elif c == 'R':
            d = (d + (n // 90)) % 4
        # F -> move forward by the given value
        elif c == 'F':
            if d == 0: x = x + n
            elif d == 1: y = y + n
            elif d == 2: x = x - n
            elif d == 3: y = y - n
            else: assert 0
        #
        else: assert 0
        print(c, n, '->', x, y, d)

    print(x, y, mandist(x, y))

# 106860
def d20122():
    input = open('inputs/2012.in').read()
    input = list(map(lambda l: (l[0], int(l[1:])), input.splitlines()))

    x, y, wx, wy = 0, 0, 10, -1
    for (c, n) in input:
        # N -> move the waypoint north by the given value
        if c == 'N':
            wy = wy - n
        # S -> move the waypoint south by the given value
        elif c == 'S':
            wy = wy + n
        # E -> move the waypoint east by the given value
        elif c == 'E':
            wx = wx + n
        # W -> move the waypoint west by the given value
        elif c == 'W':
            wx = wx - n
        # R -> rotate the waypoint around the ship right (clockwise) the given number of degrees
        # L -> rotate the waypoint around the ship left (counter-clockwise) the given number of degrees
        elif c == 'R' or c == 'L':
            d = ((n * {'R': 1, 'L': -1}[c]) // 90) % 4
            if d == 0:
                pass
            elif d == 1:
                wx, wy = -wy, wx
            elif d == 2:
                wx, wy = -wx, -wy
            elif d == 3:
                wx, wy = wy, -wx
            else: assert 0
        # F -> move forward to the waypoint a number of times equal to the given value
        elif c == 'F':
            x = x + wx * n
            y = y + wy * n
        #
        else: assert 0

    print(x, y, mandist(x, y))




# ###########################################################################
#
# 2020 DAY 11
#
# ###########################################################################

# 2263
def d20111():
    input = open('inputs/2011.in').read()
    cgrid = dict(
        ((x, y), c)
        for (y, line) in enumerate(input.splitlines())
        for (x, c) in enumerate(line.strip()))

    def nbocc(vals):
        return len(list(filter(lambda x: x == '#', vals)))

    def nextv(v, l):
        if v == 'L' and l == 0: return '#'
        if v == '#' and l >= 4: return 'L'
        return v

    def neighs(grid, x, y):
        return nbocc(
            grid.get((xn, yn)) for xn in range(x-1, x+2) for yn in range(y-1, y+2)
            if (xn, yn) != (x, y))

    ngrid = None
    while True:
        ngrid =  dict(
            (((x, y), nextv(cgrid[x, y], neighs(cgrid, x, y))) for (x, y) in cgrid))
        if ngrid == cgrid: break
        cgrid = ngrid

    print(nbocc(cgrid.values()))

# 2002
def d20112():
    input = open('inputs/2011.in').read()
    cgrid = dict(
        ((x, y), c)
        for (y, line) in enumerate(input.splitlines())
        for (x, c) in enumerate(line.strip()))

    def nbocc(vals):
        return len(list(filter(lambda x: x == '#', vals)))

    def nextv(v, l):
        if v == 'L' and l == 0: return '#'
        if v == '#' and l >= 5: return 'L'
        return v

    def neighs(grid, x, y):
        dirs = ((dx, dy) for dx in range(-1, +2) for dy in range(-1, +2) if (dx, dy) != (0, 0))
        ns = []
        for (dx, dy) in dirs:
            nx, ny = x, y
            while True:
                nx, ny = nx + dx, ny + dy
                nv = grid.get((nx, ny), None)
                if nv == None or nv == 'L' or nv == '#': break
            ns.append(nv)
        return nbocc(ns)

    ngrid = None
    while True:
        ngrid =  dict(
            (((x, y), nextv(cgrid[x, y], neighs(cgrid, x, y))) for (x, y) in cgrid))
        if ngrid == cgrid: break
        cgrid = ngrid

    print(nbocc(cgrid.values()))



# ###########################################################################
#
# 2020 DAY 10
#
# ###########################################################################

# 2070
def d20101():
    input = open('inputs/2010.in').readlines()
    input = list(map(lambda l: int(l.strip()), input))
    input = sorted(input) + [max(input) + 3]

    adapt = list(map(lambda g: len(list(g[1])), itertools.groupby(
        sorted(list(map(lambda x_y: x_y[0] - x_y[1], zip(input, [0] + list(input)))))
    )))
    print(adapt, adapt[0] * adapt[1])

# 24179327893504
def d20102():
    input = open('inputs/2010.in').readlines()
    input = sorted(list(map(lambda l: int(l.strip()), input)))

    def nbcomb(n): return ((n <= 2) and n or (n == 3) and 4 or (n == 4) and 7 or 1/0)

    adapt = map(
        lambda x_y: x_y[0] - x_y[1], zip(input + [max(input) + 3], [0] + list(input)))

    adapt = filter(lambda v_l: v_l[0] == 1, map(
        lambda v_g: (v_g[0], len(list(v_g[1]))), itertools.groupby(adapt)))

    adapt = functools.reduce(
        operator.mul, map(lambda v_l: nbcomb(v_l[1]), adapt), 1)

    print(adapt)


# ###########################################################################
#
# 2020 DAY 9
#
# ###########################################################################

# 69316178
def d20091():
    input = open('inputs/2009.in').readlines()
    input = list(map(lambda l: int(l.strip()), input))
    prmbl = 25

    print(next(input[n] for n in range(prmbl, len(input)) if (
        input[n] not in list(
            map(sum, itertools.combinations(input[n - prmbl : n], 2)))
    )))

    # for n in range(prmbl, len(input)):
    #     cur, prv = input[n], input[n - prmbl : n]
    #     if not cur in list(map(sum, itertools.combinations(prv, 2))):
    #         print(cur); break

# 9351526
def d20092():
    input = open('inputs/2009.in').readlines()
    input = list(map(lambda l: int(l.strip()), input))
    targt = 69316178

    print(next((
        min(input[x:n+1]) + max(input[x:n+1])
        for n in range(len(input)) for x in range(n, -1, -1)
        if sum(input[x : n + 1]) == targt), None))

    # for n in range(len(input)):
    #     s, m = 0, n
    #     while m >= 0 and s < targt:
    #         s += input[m]
    #         if s == targt:
    #             mn, mx = min(input[m:n+1]), max(input[m:n+1])
    #             print(mn + mx); return
    #         m -= 1


# ###########################################################################
#
# 2020 DAY 8
#
# ###########################################################################

# 2080
def d20081():
    input = open('inputs/2008.in').readlines()
    input = list(map(lambda l: tuple(l.strip().split()), input))

    pc, acc = 0, 0
    seen = set([])

    while True:
        if pc in seen: break
        seen.add(pc)

        opc, arg = input[pc]
        if opc == 'nop':
            pc += 1
        elif opc == 'acc':
            acc += int(arg)
            pc += 1
        elif opc == 'jmp':
            pc += int(arg)
        else: assert 0

    print(acc)

# 2477
def d20082():
    input = open('inputs/2008.in').readlines()
    input = list(map(lambda l: tuple(l.strip().split()), input))

    def loop(input):
        pc, acc = 0, 0
        seen = set([])
        while True:
            if pc in seen: return None
            seen.add(pc)
            opc, arg = input[pc]
            if opc == 'nop':
                pc += 1
            elif opc == 'acc':
                acc += int(arg)
                pc += 1
            elif opc == 'jmp':
                pc += int(arg)
            else: assert 0
            if pc >= len(input): break
        return acc

    for (n, (opc, arg)) in enumerate(input):
        if opc == 'nop':
            modinput = list(input)
            modinput[n] = ('jmp', arg)
            if (res := loop(modinput)) is not None:
                print(res); break
        elif opc == 'jmp':
            modinput = list(input)
            modinput[n] = ('nop', arg)
            if (res := loop(modinput)) is not None:
                print(res); break
        else: pass


# ###########################################################################
#
# 2020 DAY 7
#
# ###########################################################################

# 229
def d20071():
    input = open('inputs/2007.in').read()

    rules = dict(
        (src, re.findall('(\d*) ([^,\.]+) bag.?.\s*', re.sub('no', '0', dsts)))
        for (src, dsts) in re.findall('(?m:^(.*) bags contain (.*)\n)', input))

    def findbag(col):
        return (col == 'shiny gold') or any(
            map(lambda n_d: findbag(n_d[1]), rules.get(col, [])))

    print(len(list(filter(findbag, rules.keys()))) - 1)

# 6683
def d20072():
    input = open('inputs/2007.in').read()

    rules = dict(
        (src, re.findall('(\d*) ([^,\.]+) bag.?.\s*', re.sub('no', '0', dsts)))
        for (src, dsts) in re.findall('(?m:^(.*) bags contain (.*)\n)', input))

    def nbags(col):
        return sum((int(n) * (1 + nbags(d))) for (n, d) in rules.get(col, []))

    print(nbags('shiny gold'))


# ###########################################################################
#
# 2020 DAY 6
#
# ###########################################################################

# 6947
def d20061():
    input = open('inputs/2006.in').read()
    input = map(str.split, input.split('\n\n'))
    answr = sum(map(lambda xs: len(set(''.join(xs))), input))
    print(answr)

# 3398
def d20062():
    input = open('inputs/2006.in').read()
    input = map(str.split, input.split('\n\n'))
    answr = sum(map(lambda xs: len(
        functools.reduce(lambda acc, x: (acc & set(x)), xs, set(xs[0]))
    ), input))
    print(answr)


# ###########################################################################
#
# 2020 DAY 5
#
# ###########################################################################

def decode2005(c):
    def part(xs, min, max):
        for x in xs:
            if x == 'F' or x == 'L':
                max = (min + max) // 2
            elif x == 'B' or x == 'R':
                min = 1 + (min + max) // 2
            else: assert 0
        assert min == max
        return min
    return part(c[:7], 0, 127), part(c[7:], 0, 7)

def seatid(rs): return rs[0] * 8 + rs[1]

# 947
def d20051():
    input = open('inputs/2005.in').read().splitlines()

    print(max(map(lambda c: seatid(decode2005(c)), input)))

# 636
def d20052():
    input = open('inputs/2005.in').read().splitlines()

    brd_seats_id = list(map(lambda c: seatid(decode2005(c)), input))

    seats_id = (seatid((r, s)) for r in range(1, 127) for s in range(8))

    seats_id = list(filter(
        lambda i: ((i - 1) in brd_seats_id) and ((i + 1) in brd_seats_id),
        set(seats_id) - set(brd_seats_id)))

    print(seats_id)


# ###########################################################################
#
# 2020 DAY 4
#
# ###########################################################################

# 196
def d20041():
    input = open('inputs/2004.in').read()
    input = list(map(lambda x: dict(
        map(lambda y: y.split(':'), x.split())), input.split('\n\n')))

    reqs = set(['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']) # cid?
    valids = list(filter(lambda d: set(d.keys()).issuperset(reqs), input))

    print(len(valids))

# 114
def d20042():
    input = open('inputs/2004.in').read()
    input = list(map(lambda x: dict(
        map(lambda y: y.split(':'), x.split())), input.split('\n\n')))

    reqs = set(['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']) # cid?
    valids = list(filter(lambda d: set(d.keys()).issuperset(reqs), input))

    def isvalid(k, v):
        try:
            # byr (Birth Year) - four digits; at least 1920 and at most 2002.
            if k == 'byr':
                assert re.match('[0-9]{4}$', v)
                assert 1920 <= int(v) <= 2002
            # iyr (Issue Year) - four digits; at least 2010 and at most 2020.
            elif k == 'iyr':
                assert re.match('[0-9][0-9][0-9][0-9]$', v)
                assert 2010 <= int(v) <= 2020
            # eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
            elif k == 'eyr':
                assert re.match('[0-9][0-9][0-9][0-9]$', v)
                assert 2020 <= int(v) <= 2030
            # hgt (Height) - a number followed by either cm or in:
            #     If cm, the number must be at least 150 and at most 193.
            #     If in, the number must be at least 59 and at most 76.
            elif k == 'hgt':
                assert re.match('[0-9]+(cm|in)$', v)
                assert not v.endswith('cm') or (150 <= int(v[:-2]) <= 193)
                assert not v.endswith('in') or (59 <= int(v[:-2]) <= 76)
            # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
            elif k == 'hcl':
                assert re.match('#[0-9a-f]{6}$', v)
            # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
            elif k == 'ecl':
                assert re.match('(amb|blu|brn|gry|grn|hzl|oth)$', v)
            # pid (Passport ID) - a nine-digit number, including leading zeroes.
            elif k == 'pid':
                assert re.match('[0-9]{9}$', v)
            # cid (Country ID) - ignored, missing or not.
            elif k == 'cid':
                pass
            else:
                assert 0
        except AssertionError: return False
        return True

    valids = list(filter(lambda x: all(isvalid(*k_v) for k_v in x.items()), valids))

    print(len(valids))


# ###########################################################################
#
# 2020 DAY 3
#
# ###########################################################################

# 254
def d20031():
    input = open('inputs/2003.in').readlines()
    input = list(map(str.strip, input))

    xmax, ymax = len(input[0]), len(input)
    (x, y), slope, trees = (0, 0), (3, 1), 0
    while y < ymax:
        trees += (input[y][x % xmax] == '#') and 1 or 0
        x, y = x + slope[0], y + slope[1]
    print(trees)

# 1666768320
def d20032():
    input = open('inputs/2003.in').readlines()
    input = list(map(str.strip, input))

    xmax, ymax = len(input[0]), len(input)
    slopes, res = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)], 1
    for slope in slopes:
        (x, y), trees = (0, 0), 0
        while y < ymax:
            trees += (input[y][x % xmax] == '#') and 1 or 0
            x, y = (x + slope[0], y + slope[1])
        res *= trees
    print(res)


# ###########################################################################
#
# 2020 DAY 2
#
# ###########################################################################

# 546
def d20021():
    input = open('inputs/2002.in').readlines()
    input = map(lambda line: re.findall('[0-9a-zA-Z]+', line.strip()), input)

    def isvalid(args):
        l, h, c, s = args
        return int(l) <= len(list(filter(lambda x: x == c, s))) <= int(h)

    print(len(list(filter(isvalid, input))))

# 275
def d20022():
    input = open('inputs/2002.in').readlines()
    input = map(lambda line: re.findall('[0-9a-zA-Z]+', line.strip()), input)

    def isvalid(args):
        l, h, c, s = args
        return (s[int(l) - 1] == c) ^ (s[int(h) - 1] == c)

    print(len(list(filter(isvalid, input))))


# ###########################################################################
#
# 2020 DAY 1
#
# ###########################################################################

# 605364
def d20011():
    input = open('inputs/2001.in').read()
    input = map(int, input.strip().splitlines())

    combs = itertools.combinations(input, 2)
    combs = filter(lambda c: sum(c) == 2020, combs)

    print(functools.reduce(operator.mul, next(combs), 1))

# 128397680
def d20012():
    input = open('inputs/2001.in').read()
    input = map(int, input.strip().splitlines())

    combs = itertools.combinations(input, 3)
    combs = filter(lambda c: sum(c) == 2020, combs)

    print(functools.reduce(operator.mul, next(combs), 1))


# ###########################################################################
