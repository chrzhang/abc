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


if __name__ == '__main__':
    unittest.main()
