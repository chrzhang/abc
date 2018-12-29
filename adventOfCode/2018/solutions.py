from itertools import cycle, product, chain, count
from collections import Counter, defaultdict, deque, namedtuple
from functools import lru_cache
import re
import heapq

# Helpers


def has_n_occ(amount, text):
    ctr = Counter(text)
    return any([ct == amount for ct in dict(ctr).values()])


def differ_by_1(str1, str2):
    if len(str1) != len(str2):
        return False
    i = 0
    mismatch_found = False
    while i < len(str1):
        if str1[i] != str2[i]:
            if mismatch_found:
                return False
            mismatch_found = True
        i += 1
    return mismatch_found


def overlap(str1, str2):
    if len(str1) != len(str2):
        return None
    return ''.join([str1[i] for i in range(len(str1)) if str1[i] == str2[i]])


def mode_count(lst):
    ctr = Counter(lst)
    return ctr.most_common(1)[0]


def mode(lst):
    return mode_count(lst)[0]


def mean(lst):
    return sum(lst) / len(lst)


def manhat_dist(c_1, c_2):
    return abs(c_1[0] - c_2[0]) + abs(c_1[1] - c_2[1])


def all_dist(coord, coords):
    return sum([manhat_dist(coord, c) for c in coords])


def flatten(lst):
    return [i for sublist in lst for i in sublist]


Pos = namedtuple('Pos', ['x', 'y'])

# Solutions


def day1a(line):
    return sum([int(x) for x in line.split(', ')])


def day1b(line):
    curr_freq = 0
    freqs_seen = set([curr_freq])
    for delta in cycle(line.split(', ')):
        curr_freq += int(delta)
        if curr_freq in freqs_seen:
            return curr_freq
        freqs_seen.add(curr_freq)
    return None


def day2a(lines):
    count_2 = 0
    count_3 = 0
    for line in lines:
        if has_n_occ(2, line):
            count_2 += 1
        if has_n_occ(3, line):
            count_3 += 1
    return count_2 * count_3


def day2b(lines):
    for line1 in lines:
        for line2 in lines:
            if differ_by_1(line1, line2):
                return overlap(line1, line2)
    return None


def day3h(lines):
    fabric = defaultdict(list)
    for line in lines:
        matched = re.match(
            r'^#(?P<fid>\d+) @ (?P<col>\d+),(?P<row>\d+): (?P<width>\d+)x(?P<height>\d+)$',
            line)
        if not matched:
            raise Exception('Line malformed: {0}'.format(line))
        fid = int(matched.group('fid'))
        col = int(matched.group('col'))
        row = int(matched.group('row'))
        width = int(matched.group('width'))
        height = int(matched.group('height'))
        coords = product(range(row, row + height), range(col, col + width))
        for coord in coords:
            fabric[coord].append(fid)
    return fabric


def day3a(lines):
    fabric = day3h(lines)
    return len([k for k, v in fabric.items() if len(v) > 1])


def day3b(lines):
    fabric = day3h(lines)
    all_ids = set(flatten(fabric.values()))
    for claims in fabric.values():
        if len(claims) > 1:
            for claim in claims:
                if claim in all_ids:
                    all_ids.remove(claim)
    if len(all_ids) != 1:
        raise Exception('No answer')
    return next(iter(all_ids))


def day4h(lines):
    rebeginshift = r'^\[(?P<date>\d{4}-\d{2}-\d{2}) \d{2}:(?P<minute>\d{2})\] Guard #(?P<guard>\d+) begins shift$'  # noqa
    resleep = r'^\[(?P<date>\d{4}-\d{2}-\d{2}) \d{2}:(?P<minute>\d{2})\] falls asleep$'  # noqa
    rewake = r'^\[(?P<date>\d{4}-\d{2}-\d{2}) \d{2}:(?P<minute>\d{2})\] wakes up$'  # noqa
    curr_guard = None
    awake = 1
    asleep = 0
    curr_state = awake
    curr_sleep_interval_begin = None
    data = defaultdict(lambda: defaultdict(list))
    for line in lines:
        matched = re.match(rebeginshift, line)
        if matched:
            curr_guard = int(matched.group('guard'))
            continue
        matched = re.match(resleep, line)
        if matched:
            assert curr_state == awake
            curr_state = asleep
            curr_sleep_interval_begin = int(matched.group('minute'))
            continue
        matched = re.match(rewake, line)
        if matched:
            assert curr_state == asleep
            curr_state = awake
            for minute in range(curr_sleep_interval_begin,
                                int(matched.group('minute'))):
                data[curr_guard][matched.group('date')].append(minute)
            continue
    return data


def day4a(lines):
    data = day4h(lines)
    most_sleepy = max(data, key=lambda x: sum([len(m) for m in data[x].values()]))
    all_mins = flatten(data[most_sleepy].values())
    return most_sleepy * mode(all_mins)


def day4b(lines):
    data = day4h(lines)
    guards_to_most_sleepy_mins = {}
    for guard, dminutes in data.items():
        all_mins = flatten(dminutes.values())
        guards_to_most_sleepy_mins[guard] = mode_count(all_mins)
    chosen_guard = max(guards_to_most_sleepy_mins,
                       key=lambda x: guards_to_most_sleepy_mins[x][1])
    return chosen_guard * guards_to_most_sleepy_mins[chosen_guard][0]


def day5a(linestr):
    line = list(linestr)

    def flip(char1, char2):
        if char1.islower():
            return char2.isupper() and char2.lower() == char1
        return char2.islower() and char2.upper() == char1

    stack = deque()
    for c_r in line:
        if stack and flip(stack[-1], c_r):
            stack.pop()
        else:
            stack.append(c_r)

    return ''.join(stack)


def day5b(linestr):
    all_types = {cr.lower() for cr in linestr}
    min_len = len(linestr)
    for polyt in all_types:
        removed = filter(lambda cr, pt=polyt: cr.lower() != pt, linestr)
        min_len = min(min_len, len(day5a(removed)))
    return min_len


def day6a(coords):
    minx = min([coord[0] for coord in coords])
    maxx = max([coord[0] for coord in coords])
    miny = min([coord[1] for coord in coords])
    maxy = max([coord[1] for coord in coords])

    def get_bbox():
        bbox = {coord: set()
                for coord in product(range(minx, maxx + 1), range(miny, maxy + 1))}
        for idx, coord in enumerate(coords):
            bbox[coord] = {idx}
        frontier = set(coords)
        while frontier:
            next_frontier = set()
            for coord in frontier:
                neighbors = [n for n in [(coord[0] + 1, coord[1]),
                                         (coord[0] - 1, coord[1]),
                                         (coord[0], coord[1] - 1),
                                         (coord[0], coord[1] + 1)]
                             if n in bbox and (not bbox[n] or n in next_frontier)]
                for neighbor in neighbors:
                    bbox[neighbor] |= bbox[coord]
                    if neighbor not in next_frontier:
                        next_frontier.add(neighbor)
            frontier = next_frontier
        return bbox

    bbox = get_bbox()
    # Get letters that would be inf since they live on edge of bbox
    blacklist = set()
    edges = [coord for coord in bbox
             if len(bbox[coord]) == 1 and (coord[0] in (minx, maxx) or
                                           coord[1] in (miny, maxy))]
    for edge in edges:
        blacklist |= bbox[edge]
    contenders = chain.from_iterable([x for x in bbox.values() if len(x) == 1])
    contenders = [c for c in contenders if c not in blacklist]
    return mode_count(contenders)[1]


def day6b(coords, limit):
    meanx = mean([coord[0] for coord in coords])
    meany = mean([coord[1] for coord in coords])
    center = (int(meanx), int(meany))
    visited = set([center])
    result = 0
    frontier = [center]
    while frontier:
        next_frontier = []
        for coord in frontier:
            result += 1
            assert all_dist(coord, coords) < limit
            neighbors = [(coord[0] + 1, coord[1]),
                         (coord[0] - 1, coord[1]),
                         (coord[0], coord[1] - 1),
                         (coord[0], coord[1] + 1)]
            for neighbor in neighbors:
                if neighbor not in visited:
                    visited.add(neighbor)
                    if all_dist(neighbor, coords) < limit:
                        next_frontier.append(neighbor)
        frontier = next_frontier
    return result


def day7h(lines):
    rels = []
    for line in lines:
        matched = re.match(
            r'^Step (?P<prereq>\w) must be finished before step (?P<step>\w) can begin.$',
            line)
        if not matched:
            raise Exception('Line malformed: {0}'.format(line))
        rels.append((matched.group('prereq'), matched.group('step')))
    step_prereqs = {d: [] for d in flatten([r for r in rels])}
    for relation in rels:
        step_prereqs[relation[1]] += relation[0]
    return step_prereqs


def day7a(lines):
    step_prereqs = day7h(lines)
    done = set()
    result = ''
    while len(result) != len(step_prereqs):
        can_do = min([step for step, pqs in step_prereqs.items()
                      if step not in done and all([x in done for x in pqs])])
        done.add(can_do)
        result += can_do
    return result


def day7b(lines, num_workers, base_time):
    step_prereqs = day7h(lines)
    workers = {}
    doing = set()
    done = set()
    second = 0
    result = ''
    while len(result) != len(step_prereqs):
        # Check if any jobs are finished
        jobs_finished = []
        for step in workers:
            workers[step] += 1
            if workers[step] == base_time + (ord(step) - ord('A') + 1):
                jobs_finished.append(step)
        for j in sorted(jobs_finished):
            result += j
            doing.remove(j)
            done.add(j)
            del workers[j]
        # Add new jobs
        if num_workers - len(workers):
            can_do = [step for step, pqs in step_prereqs.items()
                      if (step not in doing and step not in done) and all([x in done for x in pqs])]
            num_jobs_taskable = min(len(can_do), num_workers - len(workers))
            if num_jobs_taskable:
                steps_tasked = can_do[:num_jobs_taskable]
                for step in steps_tasked:
                    workers[step] = 0
                    doing.add(step)
        second += 1
    return second - 1


def day8a(lst):
    def recurse(begin):
        result = 0
        child_count = lst[begin]
        meta_count = lst[begin + 1]
        i = begin + 2
        for _ in range(child_count):
            val = recurse(i)
            i = val[0]
            result += val[1]
        for _ in range(meta_count):
            result += lst[i]
            i += 1
        return (i, result)
    return recurse(0)[1]


def day8b(lst):
    def value(begin):
        result = 0
        child_count = lst[begin]
        meta_count = lst[begin + 1]
        i = begin + 2
        children_values = []
        for _ in range(child_count):
            val = value(i)
            i = val[0]
            children_values.append(val[1])
        for _ in range(meta_count):
            if not children_values:
                result += lst[i]
            elif lst[i] <= len(children_values):
                result += children_values[lst[i] - 1]
            i += 1
        return (i, result)
    return value(0)[1]


def day9(player_count, marble_count):
    scores = [0 for _ in range(player_count)]
    marbles = deque([0])
    for marble in range(1, marble_count + 1):
        if marble % 23:
            marbles.rotate(-1)
            marbles.append(marble)
        else:
            marbles.rotate(7)
            scores[marble % player_count] += marbles.pop() + marble
            marbles.rotate(-1)
    return max(scores)


def day10(disp):
    def show_disp(disp):
        miny = min(disp, key=lambda x: x[1])[1]
        maxy = max(disp, key=lambda x: x[1])[1]
        minx = min(disp, key=lambda x: x[0])[0]
        maxx = max(disp, key=lambda x: x[0])[0]
        lines = []
        offsetted = {(x[0] - minx, x[1] - miny) for x in disp}
        for col in range(maxy - miny + 1):
            line = ''
            for row in range(maxx - minx + 1):
                if (row, col) in offsetted:
                    line += 'O'
                else:
                    line += '.'
            lines.append(line)
        return lines
    second = 0
    min_so_far = None
    while True:
        miny = min(disp, key=lambda x: x[1])[1]
        maxy = max(disp, key=lambda x: x[1])[1]
        if min_so_far is None:
            min_so_far = abs(maxy - miny)
        else:
            min_so_far = min(min_so_far, abs(maxy - miny))
        # This solution does not account for setup where points expand into word
        if abs(maxy - miny) > min_so_far:  # When we're done converging
            for i, _ in enumerate(disp):
                disp[i][0] -= disp[i][2]
                disp[i][1] -= disp[i][3]
            return (show_disp(disp), second - 1)
        for i, _ in enumerate(disp):
            disp[i][0] += disp[i][2]
            disp[i][1] += disp[i][3]
        second += 1


def day12(rules, initial_state, gens, brevity=False):

    rule_len = len(next(iter(rules.keys())))

    def tick(offset_flowers):
        def tick_aux(flowers):
            next_flowers = flowers.copy()
            minflower = min(flowers)
            maxflower = max(flowers)
            for pos in range(minflower - rule_len + 1, maxflower + rule_len):
                patch = ''.join(['#' if i - (rule_len // 2) + pos in flowers else '.' for i in range(rule_len)])
                if rules.get(patch, '.') == '#' and pos not in flowers:
                    next_flowers.add(pos)
                elif rules.get(patch, '.') == '.' and pos in flowers:
                    next_flowers.remove(pos)
            return next_flowers
        curr_offset, canon_flowers = offset_flowers
        adj_flowers = {x + curr_offset for x in canon_flowers}
        adj_flowers = tick_aux(adj_flowers)
        new_offset = min(adj_flowers)
        return (new_offset, {x - new_offset for x in adj_flowers})

    assert rules.get(rule_len * '.', '.') == '.'  # Else inf flowers
    if not brevity:
        assert len(rules) == 2 ** rule_len  # All possibilities covered

    flowers = {i for i, c in enumerate(initial_state) if c == '#'}
    offset_flowers = (0, flowers)
    flowers_states = {}  # Canonical flowers to (offset, ticks)
    for ticks in range(gens):
        tupled = tuple(sorted(list(offset_flowers[1])))
        if tupled in flowers_states:  # Detected a cycle
            # Input results in an infinite cycle that is only 1 unique flower
            # combination repeating (TODO Extend to longer cycles?)
            cycle_from_offset, cycle_from_tick = flowers_states[tupled]
            final_offset = (gens - cycle_from_tick + cycle_from_offset)
            return sum([x + final_offset for x in offset_flowers[1]])
        flowers_states[tupled] = (offset_flowers[0], ticks)
        offset_flowers = tick(offset_flowers)
    return sum([x + offset_flowers[0] for x in offset_flowers[1]])


def day13(my_config):
    def make_tracks(config):
        tracks = {}
        for rowi, row in enumerate(config):
            for coli, elem in enumerate(row):
                if elem in ('|', '^', 'v'):
                    tracks[(rowi, coli)] = '|'
                elif elem in ('-', '>', '<'):
                    tracks[(rowi, coli)] = '-'
                elif elem != ' ':
                    tracks[(rowi, coli)] = elem
        return tracks

    def make_carts(config):
        carts = []
        for rowi, row in enumerate(config):
            for coli, elem in enumerate(row):
                if elem in ('<', '>', '^', 'v'):
                    carts.append({'state': 'L',
                                  'dir': elem,
                                  'pos': [rowi, coli]})
        return carts

    def move(cart, tracks):
        track = tracks[tuple(cart['pos'])]
        east = (0, 1)
        west = (0, -1)
        north = (-1, 0)
        south = (1, 0)
        offset_dict = {'>': east, '<': west, '^': north, 'v': south}
        if track == '+':
            cross_dict = {
                '>': {
                    'L': '^', 'S': '>', 'R': 'v'
                },
                '<': {
                    'L': 'v', 'S': '<', 'R': '^'
                },
                'v': {
                    'L': '>', 'S': 'v', 'R': '<'
                },
                '^': {
                    'L': '<', 'S': '^', 'R': '>'
                }
            }
            cart['dir'] = cross_dict[cart['dir']][cart['state']]
            cart['pos'][0] += offset_dict[cart['dir']][0]
            cart['pos'][1] += offset_dict[cart['dir']][1]
            cart['state'] = {'L': 'S',
                             'S': 'R',
                             'R': 'L'}[cart['state']]
            return cart
        new_dir_dict = {
            '>': {
                '-': '>', '/': '^', '\\': 'v'
            },
            '<': {
                '-': '<', '/': 'v', '\\': '^'
            },
            'v': {
                '|': 'v', '/': '<', '\\': '>'
            },
            '^': {
                '|': '^', '/': '>', '\\': '<'
            }
        }
        cart['dir'] = new_dir_dict[cart['dir']][track]
        cart['pos'][0] += offset_dict[cart['dir']][0]
        cart['pos'][1] += offset_dict[cart['dir']][1]
        return cart

    my_tracks = make_tracks(my_config)
    my_carts = make_carts(my_config)

    first_collision = None
    while len(my_carts) > 1:
        my_carts = sorted(my_carts, key=lambda cart: cart['pos'])
        crashed = set()
        for carti, cart in enumerate(my_carts):
            if carti in crashed:
                continue
            my_carts[carti] = move(cart, my_tracks)
            for cartj, othercart in enumerate(my_carts):
                if cartj == carti:
                    continue
                if othercart['pos'] == cart['pos']:
                    if first_collision is None:
                        first_collision = cart['pos']
                    crashed.add(carti)
                    crashed.add(cartj)
        my_carts = [x for i, x in enumerate(my_carts) if i not in crashed]
    return first_collision, None if not my_carts else my_carts[0]['pos']


def day14a(recipe_ct):
    recipes = [3, 7]
    ielf, jelf = 0, 1
    while len(recipes) < recipe_ct + 10:
        total = recipes[ielf] + recipes[jelf]
        if total > 9:
            recipes.append(1)
        recipes.append(total % 10)
        ielf = (ielf + recipes[ielf] + 1) % len(recipes)
        jelf = (jelf + recipes[jelf] + 1) % len(recipes)
    return ''.join([str(x) for x in recipes[recipe_ct:recipe_ct + 10]])


def day14b(target):
    recipes = [3, 7]
    ielf, jelf = 0, 1
    while True:
        total = recipes[ielf] + recipes[jelf]
        if total > 9:
            recipes.append(1)
        recipes.append(total % 10)
        ielf = (ielf + recipes[ielf] + 1) % len(recipes)
        jelf = (jelf + recipes[jelf] + 1) % len(recipes)
        if target[-1] in recipes[-2:]:
            if recipes[len(recipes) - len(target):] == target:
                return str(len(recipes) - len(target))
            if recipes[len(recipes) - len(target) - 1:-1] == target:
                return str(len(recipes) - len(target) - 1)


class Day15Helpers:
    @staticmethod
    def get_neighbors(pos):
        return ((pos[0] + 1, pos[1]), (pos[0] - 1, pos[1]),
                (pos[0], pos[1] + 1), (pos[0], pos[1] - 1))

    @staticmethod
    def opp_unit(unit_type):
        return 'g' if unit_type == 'e' else 'e'

    @staticmethod
    def in_range_of_target(unit_pos, units):
        unit_type = units[unit_pos][0]
        neighbors = [n for n in Day15Helpers.get_neighbors(unit_pos) if n in units]
        return any([units[x][0] == Day15Helpers.opp_unit(unit_type) for x in neighbors])

    @staticmethod
    def attack_from(unit_pos, units, open_spots, elf_attack_power):
        unit_type = units[unit_pos][0]
        neighbors = [n for n in Day15Helpers.get_neighbors(unit_pos) if n in units and units[n][0] == Day15Helpers.opp_unit(unit_type)]
        attack_target = min(sorted(neighbors), key=lambda n: units[n][1])
        units[attack_target][1] -= elf_attack_power if unit_type == 'e' else 3
        if units[attack_target][1] <= 0:
            del units[attack_target]
            open_spots.add(attack_target)
            return attack_target
        return None

    @staticmethod
    def get_nwes(pos):
        return [[(pos[0] - 1, pos[1]) for _ in range(2)],
                [(pos[0], pos[1] - 1) for _ in range(2)],
                [(pos[0], pos[1] + 1) for _ in range(2)],
                [(pos[0] + 1, pos[1]) for _ in range(2)]]

    @staticmethod
    def count_eg(units, eorg):
        return len(list(filter(lambda x: units[x][0] == eorg, units)))

    @staticmethod
    def move_from(unit_pos, units, open_spots):
        unit_type = units[unit_pos][0]
        frontier = [n for n in Day15Helpers.get_nwes(unit_pos) if n[0] in open_spots]
        visited = set([unit_pos])
        while frontier:
            next_frontier = []
            for pos, immed in frontier:
                if pos in visited:
                    continue
                visited.add(pos)
                if any(filter(lambda x: x in units and units[x][0] == Day15Helpers.opp_unit(unit_type), Day15Helpers.get_neighbors(pos))):
                    units[immed] = units[unit_pos]
                    del units[unit_pos]
                    open_spots.add(unit_pos)
                    open_spots.remove(immed)
                    return immed
                next_frontier.extend([(n, immed) for n in Day15Helpers.get_neighbors(pos) if n in open_spots and n not in visited])
            frontier = sorted(next_frontier, key=lambda x: x[0])
        return unit_pos

    @staticmethod
    def process(filename):
        my_units = {}
        my_open_spots = set()
        with open(filename, "r") as infile:
            lines = [l.strip() for l in infile.readlines()]
        for row, line in enumerate(lines):
            for col, elem in enumerate(line):
                if elem == 'G':
                    my_units[(row, col)] = ['g', 200]
                elif elem == 'E':
                    my_units[(row, col)] = ['e', 200]
                elif elem == '.':
                    my_open_spots.add((row, col))
                elif elem == '#':
                    pass
                else:
                    raise Exception("What is {0} in {1}?".format(elem, line))
        return my_units, my_open_spots


def day15(filename, elf_attack_power):
    my_units, my_open_spots = Day15Helpers.process(filename)

    def done(i):
        return sum(v[1] for v in my_units.values()) * i

    initial_elf_count = Day15Helpers.count_eg(my_units, 'e')
    initial_goblin_count = Day15Helpers.count_eg(my_units, 'g')
    for i in count(0):
        units_to_handle = set(my_units.keys())
        skip_this = set()
        for my_unit_pos in sorted(units_to_handle):
            e_ct = Day15Helpers.count_eg(my_units, 'e')
            g_ct = Day15Helpers.count_eg(my_units, 'g')
            if not e_ct or not g_ct:
                return done(i), initial_elf_count - e_ct, initial_goblin_count - g_ct
            if my_unit_pos in skip_this:
                continue
            if not Day15Helpers.in_range_of_target(my_unit_pos, my_units):
                my_unit_pos = Day15Helpers.move_from(my_unit_pos, my_units, my_open_spots)
            if Day15Helpers.in_range_of_target(my_unit_pos, my_units):
                died = Day15Helpers.attack_from(my_unit_pos, my_units, my_open_spots, elf_attack_power)
                if died is not None:
                    skip_this.add(died)


def day15a(filename):
    return day15(filename, 3)[0]


def day15b(filename):
    for eap in count(4):
        result = day15(filename, eap)
        if result[1] == 0:
            return result[0]
    return None


class Day16Helpers:
    @staticmethod
    def do_instr(opcode, regs, m_a, m_b, m_c):
        cregs = regs[:]
        all_instrs = {
            'a': lambda r: r[m_a] + r[m_b],
            'b': lambda r: r[m_a] + m_b,
            'c': lambda r: r[m_a] * r[m_b],
            'd': lambda r: r[m_a] * m_b,
            'e': lambda r: r[m_a] & r[m_b],
            'f': lambda r: r[m_a] & m_b,
            'g': lambda r: r[m_a] | r[m_b],
            'h': lambda r: r[m_a] | m_b,
            'i': lambda r: r[m_a],
            'j': lambda r: m_a,
            'k': lambda r: m_a > r[m_b],
            'l': lambda r: r[m_a] > m_b,
            'm': lambda r: r[m_a] > r[m_b],
            'n': lambda r: m_a == r[m_b],
            'o': lambda r: r[m_a] == m_b,
            'p': lambda r: r[m_a] == r[m_b]}
        cregs[m_c] = all_instrs[opcode](cregs)
        return cregs

    @staticmethod
    def which_opcodes_apply(inregs, outregs, ininstr):
        _, m_a, m_b, m_c = ininstr
        possible_opcodes = []
        for i in 'abcdefghijklmnop':
            if Day16Helpers.do_instr(i, inregs, m_a, m_b, m_c) == outregs:
                possible_opcodes.append(i)
        return possible_opcodes

    @staticmethod
    def update_mapping(mapping, key, value):
        mapping[key] = value
        for other_key in mapping:
            if other_key == key:
                continue
            if value in mapping[other_key]:
                mapping[other_key].remove(value)
                if len(mapping[other_key]) == 1:
                    Day16Helpers.update_mapping(mapping, other_key, next(iter(mapping[other_key])))

    @staticmethod
    def parse_line(line):
        the_list = line[8:].strip('[]')
        return [int(x) for x in the_list.split(', ')]

    @staticmethod
    def get_mapping(lines):
        mapping = {k: set('abcdefghijklmnop') for k in range(16)}
        for idx, line in enumerate(lines):
            if idx % 3 == 0:
                begin = Day16Helpers.parse_line(line)
            elif idx % 3 == 1:
                curr_instr = [int(x) for x in line.split()]
            else:
                after = Day16Helpers.parse_line(line)
                possib_opcodes = Day16Helpers.which_opcodes_apply(begin, after, curr_instr)
                mapping[curr_instr[0]] = set(possib_opcodes)
        for key, value in mapping.items():
            if len(value) == 1:
                Day16Helpers.update_mapping(mapping, key, next(iter(value)))
        return mapping


def day16a(filename_before_after):
    with open(filename_before_after, 'r') as infile:
        my_lines = [l.strip() for l in infile if l.strip()]
    result = 0
    for idx, line in enumerate(my_lines):
        if idx % 3 == 0:
            begin = Day16Helpers.parse_line(line)
        elif idx % 3 == 1:
            curr_instr = [int(x) for x in line.split()]
        else:
            after = Day16Helpers.parse_line(line)
            possib_opcodes = Day16Helpers.which_opcodes_apply(begin, after, curr_instr)
            if len(possib_opcodes) >= 3:
                result += 1
    return result


def day16b(filename_before_after, filename_instrs):
    with open(filename_before_after, 'r') as infile:
        my_lines = [l.strip() for l in infile if l.strip()]
    with open(filename_instrs, 'r') as infile:
        my_instrs = [l.strip().split() for l in infile]
    mapping = Day16Helpers.get_mapping(my_lines)
    regs = [0, 0, 0, 0]
    for instrs in my_instrs:
        instrs = [int(x) for x in instrs]
        regs = Day16Helpers.do_instr(mapping[instrs[0]], regs, instrs[1], instrs[2], instrs[3])
    return regs[0]


class Day17Helpers:
    @staticmethod
    def init_grid(filename):  # grid in (col, row): symbol
        with open(filename, 'r') as infile:
            lines = [l.strip() for l in infile.readlines()]

        grid = defaultdict(lambda: '.')
        grid[(0, 500)] = '+'

        for line in lines:
            matched = re.match(r'^(?P<axis>[xy]{1})=(?P<val>\d+), [xy]{1}=(?P<otherbegin>\d+)\.\.(?P<otherend>\d+)$', line)
            val = int(matched.group('val'))
            valiscol = matched.group('axis') == 'x'
            otherbegin = int(matched.group('otherbegin'))
            otherend = int(matched.group('otherend'))
            for coord in [(z, val) if valiscol else (val, z) for z in range(otherbegin, otherend + 1)]:
                grid[coord] = '#'
            if not matched:
                raise Exception('Could not understand {0}'.format(line))
        return grid

    @staticmethod
    def go_down(row, col, grid, stats):
        while grid[(row + 1, col)] in '|.':
            if grid[(row + 1, col)] == '.':
                grid[(row + 1, col)] = '|'
                if row + 1 >= stats['min_row']:
                    stats['waterct'] += 1
            row += 1  # Go down as far as possible
            if row == stats['max_row']:
                break
        return row, col

    @staticmethod
    def go_left(row, col, grid, stats, dumped):
        left_hit_wall = True
        while grid[(row, col - 1)] != '#':
            if grid[(row, col - 1)] == '.':
                grid[(row, col - 1)] = '|'
                if row >= stats['min_row']:
                    stats['waterct'] += 1
            col -= 1  # Go left as far as possible
            if grid[(row + 1, col)] not in '#~':
                Day17Helpers.dump_water((row, col), grid, stats, dumped)
                if grid[(row + 1, col)] not in '#~':
                    left_hit_wall = False
                    break  # Stop going left
        return left_hit_wall, row, col

    @staticmethod
    def go_right(row, col, grid, stats, dumped):
        right_hit_wall = True
        while grid[(row, col + 1)] != '#':
            if grid[(row, col + 1)] != '|':
                grid[(row, col + 1)] = '|'
                if row >= stats['min_row']:
                    stats['waterct'] += 1
            col += 1  # Go right as far as possible
            if grid[(row + 1, col)] not in '#~':
                Day17Helpers.dump_water((row, col), grid, stats, dumped)
                if grid[(row + 1, col)] not in '#~':
                    right_hit_wall = False
                    break
        return right_hit_wall, row, col

    @staticmethod
    def send_droplet(fromh, grid, stats, dumped):
        row, col = fromh
        if row >= stats['max_row'] or grid[(row + 1, col)] not in '|.':
            return
        row, col = Day17Helpers.go_down(row, col, grid, stats)
        if row == stats['max_row']:
            return
        left_hit_wall, row, col = Day17Helpers.go_left(row, col, grid, stats, dumped)
        right_hit_wall, row, col = Day17Helpers.go_right(row, col, grid, stats, dumped)
        if left_hit_wall and right_hit_wall:
            while grid[(row, col)] == '|':
                grid[(row, col)] = '~'
                stats['stillct'] += 1
                col -= 1
        return

    @staticmethod
    def dump_water(source, grid, stats, dumped):
        if source in dumped:
            return
        dumped.add(source)
        curr, still = stats['waterct'], stats['stillct']
        while True:
            Day17Helpers.send_droplet(source, grid, stats, dumped)
            if stats['waterct'] == curr and stats['stillct'] == still:
                break
            curr, still = stats['waterct'], stats['stillct']


def day17(filename):
    my_grid = Day17Helpers.init_grid(filename)
    max_row = max(my_grid, key=lambda x: x[0])[0]
    min_row = min([k for k in my_grid if my_grid[k] != '+'], key=lambda x: x[0])[0]
    stats = {'waterct': 0, 'stillct': 0, 'max_row': max_row, 'min_row': min_row}
    dumped = set()
    Day17Helpers.dump_water((0, 500), my_grid, stats, dumped)
    return stats


def day17a(filename):
    return day17(filename)['waterct']


def day17b(filename):
    return day17(filename)['stillct']


class Day18Helpers():
    @staticmethod
    def neighbors(pos):
        row, col = pos
        return [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1),
                (row + 1, col + 1), (row - 1, col - 1), (row + 1, col - 1), (row - 1, col + 1)]

    @staticmethod
    def step(grid):
        copy = {}
        for pos, elem in grid.items():
            copy[pos] = elem
            neighbors = [grid.get(n) for n in Day18Helpers.neighbors(pos)]
            if elem == '.' and neighbors.count('|') >= 3:
                copy[pos] = '|'
            elif elem == '|' and neighbors.count('#') >= 3:
                copy[pos] = '#'
            elif elem == '#' and not (neighbors.count('#') >= 1 and neighbors.count('|') >= 1):
                copy[pos] = '.'
        return copy

    @staticmethod
    def get_score(grid):
        vals = list(grid.values())
        return vals.count('#') * vals.count('|')


def day18(filename, gens):
    with open(filename, 'r') as infile:
        lines = [l.strip() for l in infile.readlines()]

    my_grid = {}
    for rowi, row in enumerate(lines):
        for coli, elem in enumerate(row):
            my_grid[(rowi, coli)] = elem

    all_gens = [my_grid]
    for gen in range(gens):
        my_grid = Day18Helpers.step(my_grid)
        try:
            ind = all_gens.index(my_grid)
            # Cycle found!
            cycle_width = gen - ind + 1
            return Day18Helpers.get_score(all_gens[(gens - ind + 1) % cycle_width + ind - 1])
        except ValueError:
            all_gens.append(my_grid)
    return Day18Helpers.get_score(my_grid)


class Day20Helpers:
    FRONTIER_BEGIN = (0, 0)  # Doesn't really matter, it's relative movement that counts

    move = {
        'N': lambda pos: (pos[0] - 1, pos[1]),
        'S': lambda pos: (pos[0] + 1, pos[1]),
        'E': lambda pos: (pos[0], pos[1] + 1),
        'W': lambda pos: (pos[0], pos[1] - 1)
    }

    @staticmethod
    def make_lookup(path):
        lookup = {}
        my_stack = deque()
        for idx, tok in enumerate(path):
            if tok == ')':
                branches = []
                while path[my_stack[-1]] == '|':
                    branches.append(my_stack[-1])
                    my_stack.pop()
                lookup[my_stack.pop()] = (branches, idx)
            elif tok in '|(':
                my_stack.append(idx)
        return lookup

    @staticmethod
    def make_adjlist(path, lookup):
        my_frontier = [Day20Helpers.FRONTIER_BEGIN]
        adjlist = defaultdict(set)

        def explore(start, indent, frontier):
            while start < len(path):
                frontier = list(set(frontier))
                if path[start] == '(':
                    new_frontier = []
                    for branch in [start] + lookup[start][0]:
                        copy_of_frontier = frontier[:]
                        explore(branch + 1, indent + 1, copy_of_frontier)
                        new_frontier += copy_of_frontier
                    frontier.clear()
                    frontier.extend(new_frontier)
                    start = lookup[start][1] + 1  # Jump past closing )
                    continue
                if path[start] in '|)':
                    return
                for idx, pos in enumerate(frontier):
                    dest = Day20Helpers.move[path[start]](pos)
                    adjlist[pos].add(dest)
                    adjlist[dest].add(pos)
                    frontier[idx] = dest
                start += 1
        explore(0, 1, my_frontier)
        return adjlist

    @staticmethod
    def farthest_bfs_depth(adjlist):
        visited = set()
        bfs_frontier = [Day20Helpers.FRONTIER_BEGIN]
        depth = 0
        while bfs_frontier:
            next_bfs_frontier = []
            for elem in bfs_frontier:
                visited.add(elem)
                for neighbor in adjlist[elem]:
                    if neighbor not in visited:
                        next_bfs_frontier.append(neighbor)
            bfs_frontier = next_bfs_frontier
            depth += 1
        return depth - 1

    @staticmethod
    def amounts_at_each_bfs_depth(adjlist):
        visited = set()
        bfs_frontier = [Day20Helpers.FRONTIER_BEGIN]
        depth = 0
        amounts_at_each_depth = {}
        while bfs_frontier:
            amounts_at_each_depth[depth] = len(bfs_frontier)
            next_bfs_frontier = []
            for elem in bfs_frontier:
                visited.add(elem)
                for neighbor in adjlist[elem]:
                    if neighbor not in visited:
                        next_bfs_frontier.append(neighbor)
            bfs_frontier = next_bfs_frontier
            depth += 1
        return amounts_at_each_depth


def day20a(path):
    path = path.strip('^$')
    lookup = Day20Helpers.make_lookup(path)
    adjlist = Day20Helpers.make_adjlist(path, lookup)
    return Day20Helpers.farthest_bfs_depth(adjlist)


def day20b(path):
    path = path.strip('^$')
    lookup = Day20Helpers.make_lookup(path)
    adjlist = Day20Helpers.make_adjlist(path, lookup)
    relevant_depths = {k: v for k, v in Day20Helpers.amounts_at_each_bfs_depth(adjlist).items() if k >= 1000}
    return sum(relevant_depths.values())


class Day22Helpers:
    ROCKY = '.'
    WET = '='
    NARROW = '|'
    TORCH = 0
    CLIMBING_GEAR = 1
    NEITHER = 2
    ALL_TOOLS = [TORCH, CLIMBING_GEAR, NEITHER]
    IMPOSSIBLE_OPTIONS = {ROCKY: NEITHER,
                          WET: TORCH,
                          NARROW: CLIMBING_GEAR}

    @staticmethod
    @lru_cache(maxsize=None)
    def geological_index(pos, target, depth):
        if pos == (0, 0):
            return 0
        if pos == target:
            return 0
        if pos.y == 0:
            return pos.x * 16807
        if pos.x == 0:
            return pos.y * 48271
        return (Day22Helpers.erosion_level(Pos(pos.x - 1, pos.y), target, depth) *
                Day22Helpers.erosion_level(Pos(pos.x, pos.y - 1), target, depth))

    @staticmethod
    @lru_cache(maxsize=None)
    def erosion_level(pos, target, depth):
        return (Day22Helpers.geological_index(pos, target, depth) + depth) % 20183

    @staticmethod
    @lru_cache(maxsize=None)
    def region_type(pos, target, depth):
        region = Day22Helpers.erosion_level(pos, target, depth) % 3
        return [Day22Helpers.ROCKY, Day22Helpers.WET, Day22Helpers.NARROW][region]

    @staticmethod
    def get_neighbors(pos):
        return filter(lambda p: p.x >= 0 and p.y >= 0, [
            Pos(pos.x + 1, pos.y), Pos(pos.x - 1, pos.y),
            Pos(pos.x, pos.y + 1), Pos(pos.x, pos.y - 1)])

    # Consider moving N, E, W, S, AND itself, with each possible gear option
    @staticmethod
    def shortest_path(source_pos, source_tool, dest_pos, dest_tool, depth):
        # To find shortest path, use Dijkstra's algorithm
        # Nodes are positions, edges are weighted by time needed to transition to that node
        prioriq = [(0, source_pos, source_tool)]
        dists = {}
        while prioriq:
            minutes, position, tool = heapq.heappop(prioriq)
            heap_key = (position, tool)
            if heap_key in dists and dists[heap_key] <= minutes:  # We can already reach position faster
                continue
            dists[heap_key] = minutes
            if (dest_pos, dest_tool) == heap_key:
                return minutes
            for possible_tool in Day22Helpers.ALL_TOOLS:  # Consider using other tools
                regiont = Day22Helpers.region_type(position, dest_pos, depth)
                if possible_tool not in (tool, Day22Helpers.IMPOSSIBLE_OPTIONS[regiont]):
                    heapq.heappush(prioriq, (7 + minutes, position, possible_tool))
            for neigh in Day22Helpers.get_neighbors(position):  # Simply move, keeping our tool
                regiont = Day22Helpers.region_type(neigh, dest_pos, depth)
                if tool != Day22Helpers.IMPOSSIBLE_OPTIONS[regiont]:
                    heapq.heappush(prioriq, (1 + minutes, neigh, tool))


def day22a(depth, targett):
    target = Pos(targett[0], targett[1])
    result = 0
    for rowi in range(target.y + 1):
        line = ''
        for coli in range(target.x + 1):
            regiont = Day22Helpers.region_type(Pos(coli, rowi), target, depth)
            line += regiont
            result += [Day22Helpers.ROCKY, Day22Helpers.WET, Day22Helpers.NARROW].index(regiont)
    return result


def day22b(depth, target):
    return Day22Helpers.shortest_path(Pos(0, 0), Day22Helpers.TORCH, target, Day22Helpers.TORCH, depth)
