#!/usr/bin/python3
"""Check that our solutions work against the inputs."""
import unittest
from solutions import *

class TestHelpers(unittest.TestCase):
    """Test helper functions."""
    pass

"""Test against website sample inputs (c), part 1 (a), and part 2 (b)."""

class TestDay1(unittest.TestCase):
    def setUp(self):
        with open('inputs/day1_input', 'r') as f:
            lines = f.read()
        self.tidylines = ', '.join(filter(None, lines.split('\n')))
    def test_day1c(self):
        self.assertEqual(3, day1a('+1, -2, +3, +1'))
        self.assertEqual(3, day1a('+1, +1, +1'))
        self.assertEqual(0, day1a('+1, +1, -2'))
        self.assertEqual(-6, day1a('-1, -2, -3'))
        self.assertEqual(2, day1b('+1, -2, +3, +1'))
    def test_day1a(self):
        self.assertEqual(439, day1a(self.tidylines))
    def test_day1b(self):
        self.assertEqual(124645, day1b(self.tidylines))

class TestDay2(unittest.TestCase):
    def setUp(self):
        with open('inputs/day2_input', 'r') as f:
            self.lines = [l.strip() for l in f.readlines()]
    def test_day2c(self):
        self.assertEqual(12, day2a(['abcdef',
                                    'bababc',
                                    'abbcde',
                                    'abcccd',
                                    'aabcdd',
                                    'abcdee',
                                    'ababab']))
    def test_day2a(self):
        self.assertEqual(5368, day2a(self.lines))
    def test_day2b(self):
        self.assertEqual('cvgywxqubnuaefmsljdrpfzyi', day2b(self.lines))


if __name__ == '__main__':
    unittest.main()
