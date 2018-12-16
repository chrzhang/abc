from itertools import cycle, product, chain, count
from collections import Counter, defaultdict, deque
import re

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
            else:
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
    all_types = set([cr.lower() for cr in linestr])
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
        offsetted = set([(x[0] - minx, x[1] - miny) for x in disp])
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
        adj_flowers = set([x + curr_offset for x in canon_flowers])
        adj_flowers = tick_aux(adj_flowers)
        new_offset = min(adj_flowers)
        return (new_offset, set([x - new_offset for x in adj_flowers]))

    assert rules.get(rule_len * '.', '.') == '.'  # Else inf flowers
    if not brevity:
        assert len(rules) == 2 ** rule_len  # All possibilities covered

    flowers = set([i for i, c in enumerate(initial_state) if c == '#'])
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
