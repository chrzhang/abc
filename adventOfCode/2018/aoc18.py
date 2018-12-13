#!/usr/bin/python3
'''Check that our solutions work against the inputs.'''
import unittest
import re
import solutions


class TestHelpers(unittest.TestCase):
    '''Test helper functions.'''
    pass


# Test against website sample inputs (c), part 1 (a), and part 2 (b).


class TestDay1(unittest.TestCase):
    def setUp(self):
        with open('inputs/day1_input', 'r') as infile:
            lines = infile.read()
        self.tidylines = ', '.join(filter(None, lines.split('\n')))

    def test_c(self):
        self.assertEqual(3, solutions.day1a('+1, -2, +3, +1'))
        self.assertEqual(3, solutions.day1a('+1, +1, +1'))
        self.assertEqual(0, solutions.day1a('+1, +1, -2'))
        self.assertEqual(-6, solutions.day1a('-1, -2, -3'))
        self.assertEqual(2, solutions.day1b('+1, -2, +3, +1'))
        self.assertEqual(0, solutions.day1b('+1, -1'))
        self.assertEqual(10, solutions.day1b('+3, +3, +4, -2, -4'))
        self.assertEqual(5, solutions.day1b('-6, +3, +8, +5, -6'))
        self.assertEqual(14, solutions.day1b('+7, +7, -2, -7, -4'))

    def test_a(self):
        self.assertEqual(439, solutions.day1a(self.tidylines))

    def test_b(self):
        self.assertEqual(124645, solutions.day1b(self.tidylines))


class TestDay2(unittest.TestCase):
    def setUp(self):
        with open('inputs/day2_input', 'r') as infile:
            self.lines = [l.strip() for l in infile.readlines()]

    def test_c(self):
        self.assertEqual(12, solutions.day2a(['abcdef',
                                              'bababc',
                                              'abbcde',
                                              'abcccd',
                                              'aabcdd',
                                              'abcdee',
                                              'ababab']))
        self.assertEqual('fgij', solutions.day2b(['abcde',
                                                  'fghij',
                                                  'klmno',
                                                  'pqrst',
                                                  'fguij',
                                                  'axcye',
                                                  'wvxyz']))

    def test_a(self):
        self.assertEqual(5368, solutions.day2a(self.lines))

    def test_b(self):
        self.assertEqual('cvgywxqubnuaefmsljdrpfzyi',
                         solutions.day2b(self.lines))


class TestDay3(unittest.TestCase):
    def setUp(self):
        with open('inputs/day3_input', 'r') as infile:
            self.lines = [l.strip() for l in infile.readlines()]

    def test_c(self):
        input_lines = ('#1 @ 1,3: 4x4',
                       '#2 @ 3,1: 4x4',
                       '#3 @ 5,5: 2x2')
        self.assertEqual(4, solutions.day3a(input_lines))
        self.assertEqual(3, solutions.day3b(input_lines))

    def test_a(self):
        self.assertEqual(101781, solutions.day3a(self.lines))

    def test_b(self):
        self.assertEqual(909, solutions.day3b(self.lines))


class TestDay4(unittest.TestCase):
    def setUp(self):
        with open('inputs/day4_input', 'r') as infile:
            self.lines = [l.strip() for l in infile.readlines()]
            self.lines.sort()

    def test_c(self):
        lines = ('[1518-11-01 00:00] Guard #10 begins shift',
                 '[1518-11-01 00:05] falls asleep',
                 '[1518-11-01 00:25] wakes up',
                 '[1518-11-01 00:30] falls asleep',
                 '[1518-11-01 00:55] wakes up',
                 '[1518-11-01 23:58] Guard #99 begins shift',
                 '[1518-11-02 00:40] falls asleep',
                 '[1518-11-02 00:50] wakes up',
                 '[1518-11-03 00:05] Guard #10 begins shift',
                 '[1518-11-03 00:24] falls asleep',
                 '[1518-11-03 00:29] wakes up',
                 '[1518-11-04 00:02] Guard #99 begins shift',
                 '[1518-11-04 00:36] falls asleep',
                 '[1518-11-04 00:46] wakes up',
                 '[1518-11-05 00:03] Guard #99 begins shift',
                 '[1518-11-05 00:45] falls asleep',
                 '[1518-11-05 00:55] wakes up')
        self.assertEqual(240, solutions.day4a(lines))
        self.assertEqual(4455, solutions.day4b(lines))

    def test_a(self):
        self.assertEqual(8950, solutions.day4a(self.lines))

    def test_b(self):
        self.assertEqual(78452, solutions.day4b(self.lines))


class TestDay5(unittest.TestCase):
    def setUp(self):
        with open('inputs/day5_input', 'r') as infile:
            self.line = infile.read().strip()

    def test_c(self):
        self.assertEqual('abc', solutions.day5a('abc'))
        self.assertEqual('abc', solutions.day5a('aZzbc'))
        self.assertEqual('abc', solutions.day5a('adZzDbc'))
        self.assertEqual('', solutions.day5a('aA'))
        self.assertEqual('', solutions.day5a('abBA'))
        self.assertEqual('abAB', solutions.day5a('abAB'))
        self.assertEqual('aabAAB', solutions.day5a('aabAAB'))
        self.assertEqual('dabCBAcaDA', solutions.day5a('dabAcCaCBAcCcaDA'))
        self.assertEqual(4, solutions.day5b('dabAcCaCBAcCcaDA'))

    def test_a(self):
        self.assertEqual(9704, len(solutions.day5a(self.line)))

    def test_b(self):
        self.assertEqual(6942, solutions.day5b(self.line))


class TestDay6(unittest.TestCase):
    def setUp(self):
        with open('inputs/day6_input', 'r') as infile:
            lines = [l.strip() for l in infile.readlines()]
        self.coords = []
        for line in lines:
            x_c, y_c = line.split(', ')
            self.coords.append((int(x_c), int(y_c)))

    def test_c(self):
        self.assertEqual(17, solutions.day6a(
            [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]))
        self.assertEqual(16, solutions.day6b(
            [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)], 32))

    def test_a(self):
        self.assertEqual(2906, solutions.day6a(self.coords))

    def test_b(self):
        self.assertEqual(50530, solutions.day6b(self.coords, 10000))


class TestDay7(unittest.TestCase):
    def setUp(self):
        with open('inputs/day7_input', 'r') as infile:
            self.lines = [l.strip() for l in infile.readlines()]

    def test_c(self):
        sample_in = [
            'Step C must be finished before step A can begin.',
            'Step C must be finished before step F can begin.',
            'Step A must be finished before step B can begin.',
            'Step A must be finished before step D can begin.',
            'Step B must be finished before step E can begin.',
            'Step D must be finished before step E can begin.',
            'Step F must be finished before step E can begin.']
        self.assertEqual('CABDFE', solutions.day7a(sample_in))
        self.assertEqual(15, solutions.day7b(sample_in, 2, 0))

    def test_a(self):
        self.assertEqual(
            'ADEFKLBVJQWUXCNGORTMYSIHPZ',
            solutions.day7a(
                self.lines))

    def test_b(self):
        self.assertEqual(1120, solutions.day7b(self.lines, 5, 60))


class TestDay8(unittest.TestCase):
    def setUp(self):
        with open('inputs/day8_input', 'r') as infile:
            self.line = [int(x) for x in infile.read().split()]

    def test_c(self):
        sample_in = [int(x)
                     for x in '2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2'.split()]
        self.assertEqual(138, solutions.day8a(sample_in))
        self.assertEqual(66, solutions.day8b(sample_in))

    def test_a(self):
        self.assertEqual(41454, solutions.day8a(self.line))

    def test_b(self):
        self.assertEqual(25752, solutions.day8b(self.line))


class TestDay9(unittest.TestCase):
    def setUp(self):
        self.player_count = 418
        self.marble_count = 71339

    def test_c(self):
        self.assertEqual(32, solutions.day9(9, 25))
        self.assertEqual(8317, solutions.day9(10, 1618))
        self.assertEqual(146373, solutions.day9(13, 7999))
        self.assertEqual(2764, solutions.day9(17, 1104))
        self.assertEqual(54718, solutions.day9(21, 6111))
        self.assertEqual(37305, solutions.day9(30, 5807))

    def test_a(self):
        self.assertEqual(
            412127,
            solutions.day9(
                self.player_count,
                self.marble_count))

    def test_b(self):
        self.assertEqual(
            3482394794,
            solutions.day9(
                self.player_count,
                100 *
                self.marble_count))


class TestDay10(unittest.TestCase):
    def prep_disp(self, filename):
        with open(filename, 'r') as infile:
            lines = [l.strip() for l in infile.readlines()]
        self.disp = []
        for line in lines:
            matched = re.match(
                r'^position=<[ ]*(?P<posx>[-]?\d+), [ ]*(?P<posy>[-]?\d+)> velocity=<[ ]*(?P<velx>[-]?\d+), [ ]*(?P<vely>[-]?\d+)>$',
                line)
            if not matched:
                raise Exception('Line malformed: {0}'.format(line))
            posx = int(matched.group('posx'))
            posy = int(matched.group('posy'))
            velx = int(matched.group('velx'))
            vely = int(matched.group('vely'))
            self.disp.append([posx, posy, velx, vely])

    def setUp(self):
        self.prep_disp('inputs/day10_input')

    def test_c(self):
        self.prep_disp('inputs/day10_sample')
        result = solutions.day10(self.disp)
        self.assertEqual(result[0], [
            'O...O..OOO',
            'O...O...O.',
            'O...O...O.',
            'OOOOO...O.',
            'O...O...O.',
            'O...O...O.',
            'O...O...O.',
            'O...O..OOO'])
        self.assertEqual(3, result[1])

    def test_ab(self):
        result = solutions.day10(self.disp)
        self.assertEqual(result[0], [
            '.OOOO...OOOOOO....OO....O....O..OOOOOO..O....O..O....O.....OOO',
            'O....O..O........O..O...OO...O..O.......O....O..O...O.......O.',
            'O.......O.......O....O..OO...O..O.......O....O..O..O........O.',
            'O.......O.......O....O..O.O..O..O.......O....O..O.O.........O.',
            'O.......OOOOO...O....O..O.O..O..OOOOO...OOOOOO..OO..........O.',
            'O..OOO..O.......OOOOOO..O..O.O..O.......O....O..OO..........O.',
            'O....O..O.......O....O..O..O.O..O.......O....O..O.O.........O.',
            'O....O..O.......O....O..O...OO..O.......O....O..O..O....O...O.',
            'O...OO..O.......O....O..O...OO..O.......O....O..O...O...O...O.',
            '.OOO.O..O.......O....O..O....O..OOOOOO..O....O..O....O...OOO..'])
        self.assertEqual(10086, result[1])


class TestDay12(unittest.TestCase):
    def setUp(self):
        self.rules = {}
        self.initial_state = '###......#.#........##.###.####......#..#####.####..#.###..#.###.#..#..#.#..#..#.##...#..##......#.#'
        with open('inputs/day12_input', 'r') as infile:
            self.lines = [l.strip() for l in infile.readlines()]
        for line in self.lines:
            matched = re.match(r'^(?P<from>[.#]+) => (?P<to>[.#]{1})$', line)
            if not matched:
                raise Exception('Line {0} malformed.'.format(line))
            self.rules[matched.group('from')] = matched.group('to')

    def test_c(self):
        sample_initial_state = '#..#.#..##......###...###'
        sample_rules = {
            '...##': '#',
            '..#..': '#',
            '.#...': '#',
            '.#.#.': '#',
            '.#.##': '#',
            '.##..': '#',
            '.####': '#',
            '#.#.#': '#',
            '#.###': '#',
            '##.#.': '#',
            '##.##': '#',
            '###..': '#',
            '###.#': '#',
            '####.': '#'}
        self.assertEqual(325, solutions.day12(sample_rules, sample_initial_state, 20, brevity=True))

    def test_a(self):
        self.assertEqual(2045, solutions.day12(self.rules, self.initial_state, 20))

    def test_b(self):
        self.assertEqual(210000000428, solutions.day12(self.rules, self.initial_state, 5 * 10 ** 9))


if __name__ == '__main__':
    unittest.main()
