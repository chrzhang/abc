import itertools
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
    for delta in itertools.cycle(line.split(', ')):
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
        coords = itertools.product(range(row, row + height),
                                   range(col, col + width))
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
                for coord in itertools.product(range(minx, maxx + 1),
                                               range(miny, maxy + 1))}
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
    contenders = itertools.chain.from_iterable([x for x in bbox.values()
                                                if len(x) == 1])
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
