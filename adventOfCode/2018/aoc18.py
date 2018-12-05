#!/usr/bin/python3
'''Check that our solutions work against the inputs.'''
import unittest
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


if __name__ == '__main__':
    unittest.main()
